{-# LANGUAGE OverloadedStrings #-}

module Frontend.Utils where

import Reflex.Dom.Core
import Control.Lens
import Control.Monad.Identity
import GHC.Generics (Generic, Generic1)
import Data.Default
import Database.Beam (tableLenses)
import Database.Beam.Schema.Tables (Beamable, zipBeamFieldsM, LensFor(..), tblSkeleton, Columnar'(..), Lenses)
import Data.Text (Text)

buttonClass :: DomBuilder t m => Text -> Text -> m (Event t ())
buttonClass cls = fmap (domEvent Click . fst) . elAttr' "button" ("type" =: "button" <> "class" =: cls) . text

buttonClassIcon :: DomBuilder t m => Text -> Text -> m (Event t ())
buttonClassIcon cls = fmap (domEvent Click . fst) . elAttr' "button" ("type" =: "button" <> "class" =: cls) . flip (elClass "i") blank

-- TODO move to reflex-dom-core and maybe explain why/when this is necessary
onRender :: (Prerender js t m, Monad m) => m (Event t ())
onRender = fmap updated (prerender blank blank)



data ConfiguratorField t m a = ConfiguratorField (Dynamic t a -> m (Dynamic t a))

instance (Beamable e, Applicative m) => Default (e (ConfiguratorField t m)) where
  def = runIdentity $ zipBeamFieldsM (\_ _ -> pure $ Columnar' $ ConfiguratorField pure) tblSkeleton tblSkeleton

distributeBeamEntity
  :: forall e m t.
  ( Reflex t
  , Generic (e (Lenses e Identity))
  , Generic (e Identity)
  , Beamable e
  , Applicative m)
  => e (Lenses e Identity)
  -> Dynamic t (e Identity)
  -> m (e (Dynamic t))
distributeBeamEntity tableLenses eD = zipBeamFieldsM op tableLenses tblSkeleton
  where 
    op :: Columnar' (Lenses e Identity) b -> w -> m (Columnar' (Dynamic t) b)
    op (Columnar' (LensFor theLens)) _ = pure $ Columnar' $ view theLens <$> eD

data ConfiguratorConfig t m e = ConfiguratorConfig
  { _configuratorConfig_fields :: e (ConfiguratorField t m)
  , _configuratorConfig_lenses :: e (Lenses e Identity)
  , _configuratorConfig_submit :: m (Event t ())
  , _configuratorConfig_warnchange :: m ()
  , _configuratorConfig_value :: Dynamic t (e Identity)
  }

entityConfigurator 
  :: forall e t m.
  (  Generic (e Identity),
     Beamable e,
     Reflex t,
     MonadHold t m,
     MonadFix m,
     Adjustable t m,
     NotReady t m,
     PostBuild t m,
     DomBuilder t m,
     Show (e Identity),
     Eq (e Identity)
  )
  => ConfiguratorConfig t m e
--  => e (Lenses e Identity)
--  -> e (ConfiguratorField t m)
--  -> m (Event t ())
--  -> Dynamic t (e Identity)
  -> m (Event t (e Identity))
entityConfigurator
  ConfiguratorConfig
    { _configuratorConfig_fields = cfg
    , _configuratorConfig_lenses = tableLenses
    , _configuratorConfig_submit = submitWidget
    , _configuratorConfig_warnchange = warnchangeWidget
    , _configuratorConfig_value = currentD
  } = mdo
    initial <- sample $ current currentD
    gatedInput <- holdDyn initial $ gate (current isUnchanged) $ updated currentD
    backendChange <- holdDyn False $ gate (not <$> current isUnchanged) (True <$ updated currentD)
    let isUnchanged = (==) <$> gatedInput <*> resultD
    results <- zipBeamFieldsM (op gatedInput) tableLenses cfg
    submitE <- submitWidget
    let resultD :: Dynamic t (e Identity) = zipBeamFieldsM (\_ (Columnar' b) -> Columnar' <$> b) cfg results
    dyn_ $ ffor backendChange $ \case
      True -> warnchangeWidget
      False -> blank
    return $ current resultD <@ submitE
  where
    op :: Dynamic t (e Identity) -> Columnar' (Lenses e Identity) b -> Columnar' (ConfiguratorField t m) b -> m (Columnar' (Dynamic t) b)
    op currentD (Columnar' (LensFor theLens)) (Columnar' (ConfiguratorField widget)) = Columnar' <$> widget (view theLens <$> currentD)

