{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Frontend.Templates.Partials.Searchbar where

import Data.Default
import Data.Text (Text)
import qualified Data.Text as T

import Reflex.Dom.Core
import Frontend.Templates.Types

searchbar
  :: DomBuilder t m
  => T.Text
  -> m (InputEl t m)
searchbar placeholder = do
  elClass "div" "mt-2 w-full shadow-button bg-white rounded flex flex-row" $ do
    elClass "button" "font-icon text-icon text-light pl-2" $ text "search"
    inputElement $ def
      & initialAttributes .~ ("class" =: "focus:outline-none flex-grow w-full h-full font-facit font-label text-label bg-transparent placeholder-light pl-1 pt-3 pb-3 pr-3"
                             <> "placeholder" =: placeholder
                             <> "type" =: "text"
                             )
