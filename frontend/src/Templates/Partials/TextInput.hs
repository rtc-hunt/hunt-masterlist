{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Templates.Partials.TextInput
  ( TextInputConfig(..)
  , TextInput(..)
  , textInput
  , textInputConfig_label
  , textInputConfig_errorMessage
  , textInputConfig_type
  , textInput_value
  , textInput_input
  ) where


import Frontend.Utils
import Control.Lens

import Data.Bool (bool)
import Data.Default
import Data.Maybe (isJust)
import qualified Data.Text as T

import Reflex.Dom.Core hiding ( TextInputConfig
                              , TextInput
                              , textInput_value
                              , textInput_input
                              , textInput
                              )

data TextInputConfig t = TextInputConfig
  { _textInputConfig_label :: T.Text
  , _textInputConfig_errorMessage :: Dynamic t (Maybe T.Text)
  , _textInputConfig_type :: T.Text
  }

instance Reflex t => Default (TextInputConfig t) where
  def = TextInputConfig "" (pure Nothing) "text"

data TextInput t = TextInput
  { _textInput_value :: Dynamic t T.Text
  , _textInput_input :: Event t T.Text
  }

textInput :: (MonadHold t m, PostBuild t m, DomBuilder t m) => TextInputConfig t -> m (TextInput t)
textInput (TextInputConfig label dErrorMessage type_) = do
  let
    dHasError = isJust <$> dErrorMessage

  elClass "div" "flex flex-col mt-4" $ do
    elClass "div" "font-facit font-label text-label" $ text label

    ie <- inputElement $ def
      & modifyAttributes .~ (("class" =:) . Just . mkInputClasses <$> updated dHasError)
      & initialAttributes .~ ( "class" =: mkInputClasses False
                             <> "placeholder" =: label
                             <> "type" =: type_
                             )

    let
      dValue = _inputElement_value ie
      eInput = _inputElement_input ie
      dFocus = _inputElement_hasFocus ie

    dHadFocus <- holdDyn False $ ffilter (==True) $ updated dFocus

    -- TODO(skylar): Do we want a different error showing behavior? Like when you have already added input and have an error
    -- maybe the message stays there?
    elDynClass "div" (mkErrorClasses <$> (zipDynWith (&&) dHadFocus (not <$> dFocus))) $ dyn_ $ ffor dErrorMessage $ \case
      Nothing -> blank
      Just err -> text err

    pure $ TextInput dValue eInput
  where
    mkErrorClasses hadFocus =
      classList [ "font-facit text-smol text-error text-opacity-70 mt-1 h-4"
                , bool "opacity-0" "opacity-100" hadFocus
                ]

    mkInputClasses hasError =
      classList [ "focus:outline-none font-facit font-label"
                , "text-label placeholder-light p-4 bg-inset"
                , "rounded shadow-input"
                , "transition-all"
                , bool "ring-primary" "ring-error focus:bg-error-inset" hasError
                , "focus:ring-4 ring-opacity-50"
                ]

makeLenses ''TextInputConfig
makeLenses ''TextInput
