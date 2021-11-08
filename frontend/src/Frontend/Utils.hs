{-# LANGUAGE OverloadedStrings #-}

module Frontend.Utils where

import qualified Data.Text as T

classList :: [T.Text] -> T.Text
classList =
  T.intercalate " "
