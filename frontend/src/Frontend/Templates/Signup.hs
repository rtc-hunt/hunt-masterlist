{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Frontend.Templates.Signup where

import Reflex.Dom.Core hiding (textInput, link)
import Obelisk.Route.Frontend

import Common.Route
import Frontend.Templates.Common
import Frontend.Templates.Partials.Header
import Frontend.Templates.Partials.TextInput

signUp
  :: ( DomBuilder t m
     , RouteToUrl (R FrontendRoute) m
     , SetRoute t (R FrontendRoute) m
     , Prerender js t m
     )
  => m ()
signUp = elClass "div" "w-screen h-screen bg-background" $ do
  header False
  elClass "div" "p-4" $ do
    h1 "mt-12" "Sign Up"

    textInput "Email"
    textInput "Profile Name"
    passwordInput

    cta "Sign Up"
    link (FrontendRoute_Login :/ ()) "Already have an account?"

