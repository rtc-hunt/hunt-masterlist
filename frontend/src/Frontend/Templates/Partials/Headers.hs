{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Frontend.Templates.Partials.Headers where

import Control.Lens
import Control.Monad
import Data.Bool
import Data.Text (Text)
import Data.Default
import Reflex.Dom.Core

import Frontend.Utils
import Frontend.Templates.Partials.Buttons

data HeaderConfig t = HeaderConfig
  { _headerConfig_header :: Text
  , _headerConfig_classes :: Text
  }

instance Default (HeaderConfig t) where
  def = HeaderConfig "H1" ""

header :: DomBuilder t m => Bool -> m ()
header showMenu = do
  elClass "div" classes $ do
    elClass "div" "" $ text "RhyoliteExample"
    when showMenu $ secondaryIconButton "" "menu" >> pure ()
  where
    classes =
      classList [ "font-karla font-bold text-copy bg-white shadow-header flex flex-row justify-between items-center z-10"
                , bool "px-4 py-5" "px-4 py-2" showMenu
                ]

h1 :: DomBuilder t m => HeaderConfig t -> m ()
h1 (HeaderConfig label cs) =
  elClass "h1" (classList ["font-karla font-bold text-h1 text-copy", cs]) $ text label

-- NOTE(skylar): There are places we use text-h2 that aren't in h2s per-se, I don't know the best way of amalgamating that
h2 :: DomBuilder t m => HeaderConfig t -> m ()
h2 (HeaderConfig label cs) =
  elClass "h2" (classList ["font-karla font-bold text-h2 text-copy", cs]) $ text label

makeLenses ''HeaderConfig
