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

data Logout = Logout

header :: DomBuilder t m => m (Event t Logout)
header = do
  elClass "div" "font-karla font-bold text-copy bg-white shadow-header flex flex-row justify-between items-center z-10 px-4 py-5" $ do
    el "div" $ text "RhyoliteExample"
    logout <- iconButton "logout"
    pure (Logout <$ logout)

h1 :: DomBuilder t m => m () -> m ()
h1 =
  elClass "h1" (classList ["font-karla font-bold text-h1 text-copy"])

h2 :: DomBuilder t m => m () -> m ()
h2 =
  elClass "h2" (classList ["font-karla font-bold text-h2 text-copy"])

makeLenses ''HeaderConfig
