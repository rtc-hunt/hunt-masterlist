{-# LANGUAGE OverloadedStrings #-}

module Frontend.Templates.Partials.Buttons where

import Reflex.Dom.Core
import qualified Data.Text as T

import Frontend.Utils

-- TODO(skylar): Should this become a config
iconButton :: DomBuilder t m => T.Text -> m (Event t ())
iconButton icon = do
  (e, _) <- elClass' "button" classes $ text icon
  pure $ domEvent Click e
  where
    classes =
      classList [ "focus:outline-none flex-shrink-0 bg-primary rounded"
                , "p-2.5 font-icon text-icon text-white leading-none shadow-button"
                ]

primaryButton :: DomBuilder t m => T.Text -> m (Event t ())
primaryButton buttonText = do
  (e, _) <- elClass' "button" classes $ text buttonText
  pure $ domEvent Click e
  where
    classes =
      classList [ "focus:outline-none w-full p-4 mt-16 shadow-button bg-primary"
                , "font-facit font-bold text-white text-body text-center rounded"
                , "hover:bg-primary-rich active:bg-primary-desaturated"
                , "focus:ring-4 ring-primary ring-opacity-50"
                ]

secondaryIconButton :: DomBuilder t m => T.Text -> T.Text -> m (Event t ())
secondaryIconButton cs icon = do
  (e, _) <- elClass' "button" classes $
    elClass "div" "font-icon leading-none text-icon text-primary-dark" $ text icon
  pure $ domEvent Click e
  where
    classes =
      classList [ "focus:outline-none rounded border border-metaline"
                , "focus:ring-4 ring-primary ring-opacity-50"
                , "p-2.5 flex-shrink-0 bg-primary-light"
                , cs
                ]


secondaryButton :: DomBuilder t m => T.Text -> T.Text -> m (Event t ())
secondaryButton cs label = do
  (e, _) <- elClass' "button" classes $
    text label
  pure $ domEvent Click e
  where
    classes =
      classList [ "w-full p-2.5 leading-none text-center rounded border border-metaline"
                , "bg-primary-light text-primary-darker font-bold font-facit focus:outline-none"
                , "focus:ring-4 ring-primary ring-opacity-50"
                , cs
                ]
