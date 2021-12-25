{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Frontend where

import Control.Category
import Control.Monad.Fix
import qualified Data.Aeson as A
import qualified Data.List as L
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import qualified Data.Witherable as W
import GHCJS.DOM (currentDocumentUnchecked)
import Obelisk.Configs
import Obelisk.Frontend
import Obelisk.Generated.Static
import Obelisk.Route
import Obelisk.Route.Frontend
import Prelude hiding ((.), id)
import Reflex.Dom.Core
import Rhyolite.Api (ApiRequest(..))
import Rhyolite.Frontend.App
import Rhyolite.Frontend.Cookie

import Common.Request
import Common.Route
import Common.Schema

import Frontend.Authentication
import Frontend.Channel (channel)
import Frontend.Types

import TemplateViewer
import Templates.Login

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

auth :: forall js t m.
  ( PostBuild t m
  , MonadHold t m
  , DomBuilder t m
  , RouteToUrl (R FrontendRoute) m
  , SetRoute t (R FrontendRoute) m
  , Prerender js t m
  )
  => Dynamic t LoginMode
  -> Event t (Maybe Text)
  -> m (Event t (Text, Text))
auth mode serverError =  do
  errors <- holdDyn Nothing serverError
  Login user pass submit <- login $ LoginConfig
    { _loginConfig_mode = mode
    , _loginConfig_switchModeLink = \t ->
      let r = ffor mode $ \case
            LoginMode_Signup -> FrontendRoute_Auth :/ AuthRoute_Login :/ ()
            LoginMode_Login -> FrontendRoute_Auth :/ AuthRoute_Signup :/ ()
      in dynRouteLink r $ dynText t
    , _loginConfig_errors = errors
    }
  pure $ tag (current $ (,) <$> value user <*> value pass) submit

-- | Handle setting the user's cookie to the given auth token (if any)
manageAuthCookie :: (DomBuilder t m, Prerender js t m)
  => Event t (Maybe AuthToken)
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
        FrontendRoute_Auth -> do
          r <- askRoute
          let mode = ffor r $ \case
                AuthRoute_Signup :/ () -> LoginMode_Signup
                AuthRoute_Login :/ () -> LoginMode_Login
          rec credentials <- auth mode $ fmap Just errors
              x :: Dynamic t (Event t (Either Text AuthToken)) <- subRoute $ \case
                AuthRoute_Login ->
                  requestingIdentity . ffor credentials $ \(user, pw) ->
                    ApiRequest_Public $ PublicRequest_Login user pw
                AuthRoute_Signup -> 
                  requestingIdentity . ffor credentials $ \(user, pw) ->
                    ApiRequest_Public $ PublicRequest_Signup user pw
              let (errors, token)  = fanEither $ switch $ current x
          redirectIfAuthenticated mAuthCookie
          pure $ Just <$> token
        FrontendRoute_Main -> do
          pb <- getPostBuild
          setRoute $ FrontendRoute_Auth :/ AuthRoute_Login :/ () <$ pb
          pure never
        ) :: forall a. FrontendRoute a -> RoutedT t a (ExampleWidget t m) (Event t (Maybe AuthToken)))
  manageAuthCookie authChange
  pure ()

redirectIfAuthenticated
  :: ( PostBuild t m
     , SetRoute t (R FrontendRoute) m
     )
  => Dynamic t (Maybe token)
  -> m ()
redirectIfAuthenticated mAuthCookie = do
  pb <- getPostBuild
  let hasAuth = W.catMaybes $ leftmost
        [ tag (current mAuthCookie) pb
        , updated mAuthCookie
        ]
  setRoute $ FrontendRoute_Channel :/ Nothing <$ hasAuth

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
