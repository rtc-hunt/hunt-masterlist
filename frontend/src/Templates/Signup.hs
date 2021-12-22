{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Templates.Signup where

import Reflex.Dom.Core hiding (textInput, link)
import Obelisk.Route.Frontend

import Common.Route
import Templates.Partials.TextInput
import Templates.Types

signUp
  :: ( DomBuilder t m
     , RouteToUrl (R FrontendRoute) m
     , SetRoute t (R FrontendRoute) m
     , Prerender js t m
     )
  => m ()
signUp = elClass "div" "w-screen h-screen bg-background" $ do
  elClass "div" "p-4 mx-auto md:w-sm" $ do
    elClass "h1" "font-karla font-bold text-h1 text-copy mt-12" $
      text "Sign Up"

    email <- emailInput
    textInput "Profile Name"
    passwordInput

    cta "Sign Up"
    link (FrontendRoute_Login :/ ()) "Already have an account?"

emailInput
  :: DomBuilder t m => m (InputEl t m)
emailInput = textInput $ def
  & textInputConfig_label .~ "Email"
  & textInputConfig_type .~ "email"
