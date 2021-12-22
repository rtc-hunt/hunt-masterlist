{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Templates.Partials.Headers (header, h1, h2) where

import Reflex.Dom.Core

import Frontend.Types
import Templates.Partials.Buttons

header :: DomBuilder t m => m (Event t Logout)
header = do
  elClass "div" "font-karla font-bold text-copy bg-white shadow-header flex flex-row justify-between items-center z-10 px-4 py-5" $ do
    el "div" $ text "RhyoliteExample"
    logout <- iconButton "logout"
    pure (Logout <$ logout)

h1 :: DomBuilder t m => m () -> m ()
h1 =
  elClass "h1" "font-karla font-bold text-h1 text-copy"

h2 :: DomBuilder t m => m () -> m ()
h2 =
  elClass "h2" "font-karla font-bold text-h2 text-copy"
