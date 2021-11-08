{-# LANGUAGE OverloadedStrings #-}

module Frontend.Templates.Partials.Lists where

import Reflex.Dom.Core
import qualified Data.Text as T

-- TODO(skylar): Config?
listItem :: DomBuilder t m => T.Text -> Maybe T.Text -> m ()
listItem label mSubtext = do
  elClass "div" "flex flex-col py-2 border-b border-metaline" $ do
    elClass "div" "leading-none font-facit text-body text-copy" $ text label
    case mSubtext of
      Just subtext -> elClass "div" "mt-1 leading-none font-facit text-label text-light" $ text subtext
      Nothing -> blank
