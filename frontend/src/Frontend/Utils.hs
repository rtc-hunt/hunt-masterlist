{-# LANGUAGE OverloadedStrings #-}

module Frontend.Utils where

import Data.Text (Text)
import qualified Data.Text as T
import Reflex.Dom.Core

classList :: [Text] -> Text
classList = T.intercalate " "

-- TODO move to reflex-dom-core and maybe explain why/when this is necessary
onRender :: (Prerender js t m, Monad m) => m (Event t ())
onRender = fmap updated (prerender blank blank)


