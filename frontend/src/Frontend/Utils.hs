{-# LANGUAGE OverloadedStrings #-}

module Frontend.Utils where

import Data.Text (Text)
import qualified Data.Text as T

classList :: [Text] -> Text
classList = T.intercalate " "
