{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Frontend where

import Control.Monad
import Control.Monad.Fix
import Data.Functor.Const
import Data.Functor.Identity
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Vessel.Class
import Language.Javascript.JSaddle (eval, liftJSM)

import Obelisk.Frontend
import Obelisk.Configs
import Obelisk.Route
import Obelisk.Route.Frontend
import Obelisk.Generated.Static
import Rhyolite.Account
import Rhyolite.Frontend.App
import Rhyolite.Sign

import Reflex.Dom.Core

import Common.Api
import Common.Route
import Common.View

-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Obelisk Minimal Example"
      elAttr "link" ("href" =: $(static "main.css") <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
  , _frontend_body = runExampleWidget frontendBody
  }

frontendBody
  :: forall js t m token.
     ( Adjustable t m
     , MonadHold t m
     , MonadFix m
     , Request m ~ ExampleRequest token
     , DomBuilder t m
     , Requester t m
     , Prerender js t m
     , HasConfigs m
     )
  => RoutedT t (R FrontendRoute) m ()
frontendBody = subRoute_ $ \case
  FrontendRoute_SignUp -> do
    username <- el "div" . el "label" $ do
      text "Email"
      fmap value $ inputElement def
    password <- el "div" . el "label" $ do
      text "Password"
      fmap value . inputElement $
        def & inputElementConfig_elementConfig
            . elementConfig_initialAttributes
            .~ (AttributeName Nothing "type" =: "password")
    signupSubmit <- el "div" . el "label" $ do
      text "\160"
      signupClick <- button "Sign Up"
      -- We tag the Event of clicks on the log in button with the current values of
      -- the username and password text fields.
      return $ tag (current (zipDyn username password)) signupClick
    -- Here, we translate the signup submit events into API requests to log in.
    signupResponse <- requesting . ffor signupSubmit $ \(user, pw) ->
      ExampleRequest_Public $ PublicRequest_SignUp user pw
    pure ()
  FrontendRoute_Login -> do
    username <- el "div" . el "label" $ do
      text "Email"
      fmap value $ inputElement def
    password <- el "div" . el "label" $ do
      text "Password"
      fmap value . inputElement $
        def & inputElementConfig_elementConfig
            . elementConfig_initialAttributes
            .~ (AttributeName Nothing "type" =: "password")
    loginSubmit <- el "div" . el "label" $ do
      text "\160"
      loginClick <- button "Log In"
      -- We tag the Event of clicks on the log in button with the current values of
      -- the username and password text fields.
      return $ tag (current (zipDyn username password)) loginClick
    -- Here, we translate the login submit events into API requests to log in.
    _ <- requesting . ffor loginSubmit $ \(user, pw) ->
      ExampleRequest_Public $ PublicRequest_Login user pw
    pure ()
  FrontendRoute_Main -> do
    el "h1" $ text "Welcome to Obelisk!"
    el "p" $ text $ T.pack commonStuff
    -- `prerender` and `prerender_` let you choose a widget to run on the server
    -- during prerendering and a different widget to run on the client with
    -- JavaScript. The following will generate a `blank` widget on the server and
    -- print "Hello, World!" on the client.
    prerender_ blank $ liftJSM $ void $ eval ("console.log('Hello, World!')" :: T.Text)

    elAttr "img" ("src" =: $(static "obelisk.jpg")) blank
    el "div" $ do
      exampleConfig <- getConfig "common/example"
      case exampleConfig of
        Nothing -> text "No config file found in config/common/example"
        Just s -> text $ T.decodeUtf8 s
    pure ()

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
      (RhyoliteWidget
         ((AuthenticatedV (Signed (AuthToken Identity)) PrivateChatV) (Const SelectedCount))
         (ExampleRequest (Signed (AuthToken Identity))) t m)
      a
  -- ^ Child widget
  -> RoutedT t (R FrontendRoute) m a
runExampleWidget = fmap snd . runObeliskRhyoliteWidget
  vesselToWire
  "common/route"
  checkedFullRouteEncoder
  (BackendRoute_Listen :/ ())

authQueryDyn
  :: ( MonadQuery t (ErrorV e v (Const SelectedCount)) m
     , Reflex t
     , EmptyView v
     )
  => Dynamic t (ErrorV e v (Const SelectedCount))
  -> m (Dynamic t (Either e (v Identity)))
authQueryDyn = fmap (fmap observeErrorV) . queryDyn

{-
authWidget
  :: forall m t a publicRequest privateRequest cred v.
     ( Ord cred
     , MonadFix m, Reflex t, PostBuild t m
     , QueryResult (v (Const SelectedCount)) ~ v Identity
     , Semigroup (v Identity)
     , Group (v (Const SelectedCount))
     , Additive (v (Const SelectedCount))
     , View v
     )
  => cred
  -> RequesterT t (ApiRequest () publicRequest privateRequest) Identity m a
  -> QueryT t (v (Const SelectedCount))
       (RequesterT t (ApiRequest () publicRequest privateRequest) Identity m) a
  -> RhyoliteWidget
       (AuthenticatedV cred v (Const SelectedCount))
       (ApiRequest cred publicRequest privateRequest) t m (Dynamic t a)
authWidget cred errorChild authenticatedChild = RhyoliteWidget $ do
  v <- askQueryResult
  (a, vs) <- lift $ withRequesterT authenticateReq id $ runQueryT
    (withQueryT (authenticatedQueryMorphism cred) child) v
  pure undefined
 where
   authenticateReq
     :: forall x. ApiRequest () publicRequest privateRequest x
     -> ApiRequest cred publicRequest privateRequest x
   authenticateReq = \case
     ApiRequest_Public a -> ApiRequest_Public a
     ApiRequest_Private () a -> ApiRequest_Private cred a
   child = do
     pb <- getPostBuild
     v <- askQueryResult
     ev <- eitherDyn (fmap observeErrorV v)
     widgetHold 
-}
