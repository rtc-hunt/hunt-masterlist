{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Frontend.Templates.Partials.Headers where

import Reflex.Dom.Core
import qualified Data.Text as T
import Data.Default
import Control.Lens

import Frontend.Utils

data HeaderConfig t = HeaderConfig
  { _headerConfig_header :: T.Text
  , _headerConfig_classes :: T.Text
  }

instance Default (HeaderConfig t) where
  def = HeaderConfig "H1" ""

h1 :: DomBuilder t m => HeaderConfig t -> m ()
h1 (HeaderConfig label cs) =
  elClass "h1" (classList ["font-karla font-bold text-h1 text-copy", cs]) $ text label

-- NOTE(skylar): There are places we use text-h2 that aren't in h2s per-se, I don't know the best way of amalgamating that
h2 :: DomBuilder t m => HeaderConfig t -> m ()
h2 (HeaderConfig label cs) =
  elClass "h2" (classList ["font-karla font-bold text-h2 text-copy", cs]) $ text label

makeLenses ''HeaderConfig
