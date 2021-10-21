{-# LANGUAGE OverloadedStrings #-}

module Frontend.Templates.Partials.TextInput where

import qualified Data.Text as T
import Reflex.Dom.Core


data TextInputConfig t = TextInputConfig
  {
    _textInputConfig_label :: T.Text
  , _textInputConfig_errorMessage :: Maybe T.Text
  }

textInput :: DomBuilder t m => T.Text -> m ()
textInput label = do
  elClass "div" "flex flex-col mt-8" $ do
    elClass "div" "font-facit font-label text-label" $ text label
    _ <- inputElement $ def
      & initialAttributes .~ ("class" =: "focus:outline-none font-facit font-label text-label placeholder-light p-4 bg-inset rounded shadow-input"
                             <> "placeholder" =: label
                             <> "type" =: "text"
                             )
    pure ()
