{-# LANGUAGE OverloadedStrings #-}

module Frontend.Utils where

import Data.Map (Map)
import Data.Map.Monoidal (getMonoidalMap)
import qualified Data.Text as T
import Rhyolite.SemiMap (SemiMap, getComplete)

classList :: [T.Text] -> T.Text
classList =
  T.intercalate " "

completeMapOf
  :: SemiMap k a
  -> Maybe (Map k a)
completeMapOf = fmap getMonoidalMap . getComplete
