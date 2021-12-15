{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Frontend.Templates.Partials.PasswordInput where

import Frontend.Utils

import Control.Lens
import Control.Monad.Fix

import Data.Bool (bool)
import Data.Default
import Data.Maybe (isJust)
import qualified Data.Text as T

import Reflex.Dom.Core

data PasswordInput t = PasswordInput
  { _passwordInput_value :: Dynamic t T.Text
  , _passwordInput_input :: Event t T.Text
  }

data PasswordInputConfig t = PasswordInputConfig
  { _passwordInputConfig_error :: Dynamic t (Maybe T.Text)
  }

instance Reflex t => Default (PasswordInputConfig t) where
  def = PasswordInputConfig (pure Nothing)

passwordInput :: ( MonadHold t m
                 , DomBuilder t m
                 , MonadFix m
                 , PostBuild t m
                 ) => PasswordInputConfig t -> m (PasswordInput t)
passwordInput (PasswordInputConfig dErrorMessage) = do
  let
    dHasError = isJust <$> dErrorMessage
  elClass "div" "flex flex-col mt-8" $ mdo
    elClass "div" "font-facit font-label text-label" $ text "Passphrase"

    dHadFocus <- holdDyn False $ ffilter (==True) $ updated dFocus

    (ret, dFocus) <- elDynClass "div" (mkOuterClasses <$> dHasError) $ mdo


      ie <- inputElement $ def
        & initialAttributes .~ ( "class" =: inputClasses
                               <> "placeholder" =: "Passphrase"
                               <> "type" =: "password"
                               )
        & modifyAttributes .~ ((\x -> "type" =: Just (inputType x)) <$> updated dVisible)

      let
        dValue = _inputElement_value ie
        eInput = _inputElement_input ie

        eToggle = domEvent Click toggleButton

        inputType = bool "password" "text"

      dVisible <- foldDyn ($) False $ not <$ eToggle

      -- TODO(skylar): Do we want to show current status or status on click???
      (toggleButton, _)  <- elDynClass' "button" (mkIconClasses <$> dHasError) $ dynText $ ffor dVisible $ \case
        True -> "visibility_off"
        False -> "visibility"

      pure (PasswordInput dValue eInput, _inputElement_hasFocus ie)

    elDynClass "div" (mkErrorClasses <$> (zipDynWith (&&) dHadFocus (not <$> dFocus))) $ dyn_ $ ffor dErrorMessage $ \case
      Nothing -> blank
      Just err -> text err
    pure ret

  where
    mkIconClasses hasError =
      classList [ "focus:outline-none font-icon text-icon px-4"
                , bool "text-primary-darker" "text-error opacity-70" hasError
                ]

    mkErrorClasses hadFocus =
      classList [ "font-facit text-smol text-error"
                , "text-opacity-70 mt-1 h-4"
                , bool "opacity-0" "opacity-100" hadFocus
                ]

    mkOuterClasses hasError =
      classList [ "focus-within:ring-4 ring-opacity-50 bg-inset"
                , bool "ring-primary" "ring-error focus:bg-error-inset" hasError
                , "transition-all"
                , "rounded shadow-input flex flex-row items-center"
                , "overflow-hidden"
                ]

    inputClasses =
      classList [ "focus:outline-none flex-grow w-full h-full"
                , "font-facit font-label text-label bg-transparent placeholder-light p-4"
                ]

makeLenses ''PasswordInput
makeLenses ''PasswordInputConfig
