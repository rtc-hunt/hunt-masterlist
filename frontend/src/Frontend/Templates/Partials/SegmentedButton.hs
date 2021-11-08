{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}

module Frontend.Templates.Partials.SegmentedButton where

import Reflex.Dom.Core

import Data.List
import Data.Bool
import Data.Default
import qualified Data.Text as T

import Frontend.Utils

import Control.Monad.Fix
import Control.Lens

data SegmentedButtonConfig t = SegmentedButtonConfig
  { _segmentedButtonConfig_segments :: [T.Text]
  , _segmentedButtonConfig_classes :: T.Text
  }

-- TODO(skylar): What defaults do we actually want here?
instance Default (SegmentedButtonConfig t) where
  def = SegmentedButtonConfig [] ""

segmentedButton :: (MonadHold t m, PostBuild t m, DomBuilder t m, MonadFix m) => SegmentedButtonConfig t -> m (Dynamic t T.Text)
segmentedButton (SegmentedButtonConfig segments cs) = do
  elClass "div" ( classList ["font-facit text-label p-0.3 rounded bg-inset shadow-input flex flex-row w-auto", cs] ) $ mdo
    dSelected <- holdDyn (maybe "" id $ preview folded segments) $ leftmost clickEvs
    clickEvs <- mapM id $ intersperse (separator >> pure never) $ fmap (segment dSelected) segments
    pure dSelected
  where
    separator = elClass "div" "w-px mx-0.3 py-2 bg-metaline" blank

    segment dSelected name = do
      let
        dIsSelected = (== name) <$> dSelected

        mkClasses b =
          classList [ "p-2 rounded"
                    , bool "bg-transparent" "bg-white shadow-small" b
                    ]

      (e, _) <- elDynClass' "div" (mkClasses <$> dIsSelected) $ text name
      pure $ name <$ domEvent Click e


makeLenses ''SegmentedButtonConfig
