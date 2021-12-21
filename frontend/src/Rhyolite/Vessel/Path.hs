{-# Language FlexibleInstances #-}
{-# Language FunctionalDependencies #-}
{-# Language RankNTypes #-}
{-# Language GADTs #-}
module Rhyolite.Vessel.Path
  (semiMapP, semiMapsP, FullPath, watch, module Data.Vessel.Path)
  where

import Control.Monad.Fix
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Map.Monoidal
import Data.Vessel
import Data.Vessel.Path
import Reflex
import Rhyolite.Frontend.App
import Rhyolite.SemiMap (SemiMap, getComplete)

-- * Stuff that will end up in rhyolite

semiMapP :: (Traversable f) => Path x x (f (SemiMap k v)) (f (Map k v))
semiMapP = postMap (traverse (fmap getMonoidalMap . getComplete))

semiMapsP :: (Traversable f) => Path x x (f (Map k (SemiMap k' v))) (f (Map k (Map k' v)))
semiMapsP = postMap (traverse (Just . Map.mapMaybe (fmap getMonoidalMap . getComplete)))

-- merely a formality at this point
type FullPath a v b = Path (Const SelectedCount a) (v (Const SelectedCount)) (v Identity) (Identity b)


watch :: forall t v a b m.
  ( Reflex t
  , MonadQuery t (v (Const SelectedCount)) m
  , QueryResult (v (Const SelectedCount)) ~ v Identity
  , MonadHold t m
  , MonadFix m
  , Eq (v Identity)
  )
  => Dynamic t (FullPath a v b)
  -> m (Dynamic t (Maybe b))
watch pathDyn = do
  r <- watchViewSelector . ffor pathDyn $ \path -> _path_to path (Const 1)
  return $ fmap (fmap runIdentity) . _path_from <$> pathDyn <*> r
