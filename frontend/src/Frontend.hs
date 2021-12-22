{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Frontend where

import Prelude hiding (id, (.))
import Control.Category
import Control.Monad.Fix
import qualified Data.Aeson as A
import Data.Functor.Identity
import qualified Data.List as L
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Witherable as W
import GHCJS.DOM (currentDocumentUnchecked)
import Obelisk.Frontend
import Obelisk.Configs
import Obelisk.Route
import Obelisk.Route.Frontend
import Obelisk.Generated.Static
import Reflex.Dom.Core hiding ( link
                              , textInput
                              , TextInputConfig
                              , TextInput(..)
                              , textInput_value
                              , textInput_input
                              , _textInput_value
                              )
import Rhyolite.Account
import Rhyolite.Api (ApiRequest(..))
import Rhyolite.Frontend.App
import Rhyolite.Frontend.Cookie
import Data.Signed (Signed)

import Common.Request
import Common.Route

import Frontend.Authentication
import Frontend.Channel (channel)
import Frontend.Types

import Templates.Partials.Containers
import Templates.Partials.TextInput
import Templates.Partials.PasswordInput
import Templates.Partials.Buttons
import TemplateViewer

authCookieName :: Text
authCookieName = "auth"

-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Rhyolite Example"
      elAttr "link" ("rel" =: "stylesheet" <> "href" =: "https://use.typekit.net/csf8rij.css") blank
      elAttr "link" ("href" =: $(static "css/styles.css") <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
      elAttr "link" ("rel" =: "preconnect" <> "href" =: "https://fonts.googleapis.com") blank
      elAttr "link" ("rel" =: "preconnect" <> "href" =: "https://fonts.gstatic.com" <> "crossorigin" =: "") blank
      elAttr "link" ("rel" =: "stylesheet" <> "href" =: "https://fonts.googleapis.com/css2?family=Karla:ital,wght@0,200;0,300;0,400;0,500;0,600;0,700;0,800;1,200;1,300;1,400;1,500;1,600;1,700;1,800&display=swap") blank
      elAttr "link" ("rel" =: "stylesheet" <> "href" =: "https://fonts.googleapis.com/icon?family=Material+Icons") blank
      elAttr "meta" ("name"=:"viewport" <> "content" =: "width=device-width, initial-scale=1") blank
  , _frontend_body = runExampleWidget frontendBody
  }

link :: (DomBuilder t m, RouteToUrl route m, SetRoute t route m, Prerender js t m) => route -> T.Text -> m ()
link route label = do
  routeLink route $ elClass "div" "font-facit font-label underline text-label text-link text-center mt-4" $ text label

logIn
  :: forall js t m. (PostBuild t m, MonadFix m, MonadHold t m, DomBuilder t m, RouteToUrl (R FrontendRoute) m, SetRoute t (R FrontendRoute) m, Prerender js t m)
  => Event t (Maybe Text)
  -> m (Event t (Text, Text))
logIn serverError = screenContainer $ do
  elClass "div" "p-4 mx-auto md:w-sm" $ mdo
    elClass "h1" "font-karla font-bold text-h1 text-copy mt-12" $
      text "Log In"

    let
      usernameEvent = _textInput_input ti
      passwordEvent = _passwordInput_input pi

    dError <- holdDyn Nothing $ testValidation <$> usernameEvent
    ti <- textInput $ (def :: TextInputConfig t)
      & textInputConfig_label .~ "Email/Profile Name"
      & textInputConfig_errorMessage .~ dError

    dPError <- holdDyn Nothing $ leftmost [passwordValidation <$> passwordEvent, serverError]
    pi <- passwordInput $ (def :: PasswordInputConfig t)
      & passwordInputConfig_error .~ dPError

    click <- primaryButton "Log In"
    link (FrontendRoute_Signup :/ ()) "Don't have an account?"
    credentials <- zipDyn <$> holdDyn "" usernameEvent <*> holdDyn "" passwordEvent
    pure $ W.filter testCredentials $ tagPromptlyDyn credentials click

testCredentials :: (T.Text, T.Text) -> Bool
testCredentials (user, pass) = all isNothing [testValidation user, passwordValidation pass]

passwordValidation :: T.Text -> Maybe T.Text
passwordValidation t
  | T.length t < 3 = Just "Not a valid password"
  | otherwise = Nothing

testValidation :: T.Text -> Maybe T.Text
testValidation t
  | T.length t < 3 = Just "This isn't a valid email"
  | otherwise = Nothing

signup :: forall js t m. (MonadFix m, MonadHold t m, PostBuild t m, DomBuilder t m, RouteToUrl (R FrontendRoute) m, SetRoute t (R FrontendRoute) m, Prerender js t m) => m (Event t (Text, Text))
signup = screenContainer $ do
  elClass "div" "p-4 mx-auto md:w-sm" $ do
    elClass "h1" "font-karla font-bold text-h1 text-copy mt-12" $
      text "Sign Up"

    newAccount <- textInput $ def
      & textInputConfig_label .~ "Email"
      & textInputConfig_type .~ "email"

    newPass <- passwordInput def

    let
      usernameEvent = _textInput_input newAccount
      passwordEvent = _passwordInput_input newPass

    click <- primaryButton "Sign Up"
    link (FrontendRoute_Login :/ ()) "Already have an account?"
    credentials <- zipDyn <$> holdDyn "" usernameEvent <*> holdDyn "" passwordEvent
    pure $ W.filter testCredentials $ tagPromptlyDyn credentials click

-- | Handle setting the user's cookie to the given auth token (if any)
manageAuthCookie :: (DomBuilder t m, Prerender js t m)
  => Event t (Maybe (Signed (AuthToken Identity)))
  -> m ()
manageAuthCookie authChange = do
  prerender_ blank $ do
    doc <- currentDocumentUnchecked
    performEvent_ $ ffor authChange $ \newAuth -> do
      cookie <- defaultCookieJson authCookieName newAuth
      setPermanentCookie doc cookie

frontendBody
  :: forall js t m.
     ( ObeliskWidget js t (R FrontendRoute) m
     )
  => RoutedT t (R FrontendRoute) (ExampleWidget t m) ()
frontendBody = do
  cookies <- askCookies
  let mAuthCookie0 = A.decodeStrict =<< L.lookup (T.encodeUtf8 authCookieName) cookies
  rec mAuthCookie <- holdDyn mAuthCookie0 authChange
  -- We have to give a type signature to the argument of subRoute because
  -- (at least on GHC 8.6.5) the compiler cannot properly infer that
  -- this function has the proper higher rank type.
      authChange <- fmap switchDyn $ subRoute $ ((\case
        FrontendRoute_Templates -> templateViewer >> return never
        FrontendRoute_Channel -> authenticateWithToken mAuthCookie $ do
          logout <- channel
          pure $ Nothing <$ logout
        FrontendRoute_Signup -> do
          credentials <- signup
          redirectIfAuthenticated mAuthCookie $ do
            (_loginFailed, loginSuccess) <- fmap fanEither . requestingIdentity . ffor credentials $ \(user, pw) ->
              ApiRequest_Public $ PublicRequest_Signup user pw
            pure (fmap Just loginSuccess)
        FrontendRoute_Login -> mdo
          credentials <- logIn loginError
          (loginError, cookie) <- redirectIfAuthenticated mAuthCookie $ do
            (loginFailed, loginSuccess) <- fmap fanEither . requestingIdentity . ffor credentials $ \(user, pw) ->
              ApiRequest_Public $ PublicRequest_Login user pw
            pure (fmap Just loginFailed, fmap Just loginSuccess)
          pure cookie
        FrontendRoute_Main -> do
          pb <- getPostBuild
          setRoute $ FrontendRoute_Login :/ () <$ pb
          pure never
        ) :: forall a. FrontendRoute a -> RoutedT t a (ExampleWidget t m) (Event t (Maybe (Signed (AuthToken Identity)))))
  manageAuthCookie authChange
  pure ()

redirectIfAuthenticated
  :: ( PostBuild t m
     , SetRoute t (R FrontendRoute) m
     )
  => Dynamic t (Maybe token)
  -> m a
  -> m a
redirectIfAuthenticated mAuthCookie w = do
  pb <- getPostBuild
  setRoute $ FrontendRoute_Channel :/ Nothing <$ catMaybes (leftmost [ tag (current mAuthCookie) pb, updated mAuthCookie ])
  w

runExampleWidget
  :: ( DomBuilder t m
     , HasConfigs m
     , TriggerEvent t m
     , PerformEvent t m
     , MonadHold t m
     , PostBuild t m
     , MonadFix m
     , Prerender x t m
     )
  => RoutedT t
      (R FrontendRoute)
      (ExampleWidget t m)
      a
  -- ^ Child widget
  -> RoutedT t (R FrontendRoute) m a
runExampleWidget = fmap snd . runObeliskRhyoliteWidget
  vesselToWire
  "common/route"
  checkedFullRouteEncoder
  (BackendRoute_Listen :/ ())
