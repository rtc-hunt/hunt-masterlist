{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Frontend where

import Control.Monad
import Control.Monad.Fix
import qualified Data.Aeson as A
import Data.Functor.Const
import Data.Functor.Identity
import qualified Data.List as L
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Vessel.Class
import GHCJS.DOM (currentDocumentUnchecked)
import Language.Javascript.JSaddle (eval, liftJSM)

import Obelisk.Frontend
import Obelisk.Configs
import Obelisk.Route
import Obelisk.Route.Frontend
import Obelisk.Generated.Static
import Rhyolite.Account
import Rhyolite.Api (ApiRequest(..))
import Rhyolite.Frontend.App
import Rhyolite.Frontend.Cookie
import Rhyolite.Sign

import Reflex.Dom.Core

import Common.Api
import Common.Route
import Common.View

authCookieName :: Text
authCookieName = "auth"

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
     ( ObeliskWidget js t (R FrontendRoute) m
     , Requester t m, Request m ~ ApiRequest token PublicRequest PrivateRequest
     , Response m ~ Identity
     )
  => RoutedT t (R FrontendRoute) m ()
frontendBody = do
  cookies <- askCookies
  let mAuthCookie0 = A.decodeStrict =<< L.lookup (T.encodeUtf8 authCookieName) cookies
  rec mAuthCookie <- holdDyn mAuthCookie0 authChange
  -- We have to give a type signature to the argument of subRoute because
  -- (at least on GHC 8.6.5) the compiler cannot properly infer that
  -- this function has the proper higher rank type.
      authChange <- fmap switchDyn $ subRoute $ ((\case
        FrontendRoute_SignUp -> redirectIfAuthenticated mAuthCookie $ do
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
            return $ tag (current (zipDyn username password)) signupClick
          (_signupFailed, signupSuccess) <- fmap fanEither . requestingIdentity . ffor signupSubmit $ \(user, pw) ->
            ApiRequest_Public $ PublicRequest_SignUp user pw
          pure (fmap Just signupSuccess)
        FrontendRoute_Login -> redirectIfAuthenticated mAuthCookie $ do
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
            return $ tag (current (zipDyn username password)) loginClick
          (_loginFailed, loginSuccess) <- fmap fanEither . requestingIdentity . ffor loginSubmit $ \(user, pw) ->
            ApiRequest_Public $ PublicRequest_Login user pw
          pure (fmap Just loginSuccess)
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
          pure never
        ) :: forall a. FrontendRoute a -> RoutedT t a m (Event t (Maybe (Signed (AuthToken Identity)))))
  -- Handle setting the cookies on auth change if we're running in the browser
  prerender_ (pure ()) $ do
    doc <- currentDocumentUnchecked
    performEvent_ $ ffor authChange $ \newAuth -> do
      cookie <- defaultCookieJson authCookieName newAuth
      setPermanentCookie doc cookie
    pure ()
  pure ()

testCase :: Applicative m => FrontendRoute a -> RoutedT t a m ()
testCase = \case
    FrontendRoute_Main -> pure ()
    FrontendRoute_Login -> pure ()
    FrontendRoute_SignUp -> pure ()

redirectIfAuthenticated
  :: ( PostBuild t m
     , SetRoute t (R FrontendRoute) m
     )
  => Dynamic t (Maybe token)
  -> m a
  -> m a
redirectIfAuthenticated mAuthCookie w = do
  pb <- getPostBuild
  setRoute $ FrontendRoute_Main :/ () <$ leftmost [ tag (current mAuthCookie) pb, updated mAuthCookie ]
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
      (RhyoliteWidget
         ((AuthenticatedV (Signed (AuthToken Identity)) PrivateChatV) (Const SelectedCount))
         (ApiRequest (Signed (AuthToken Identity)) PublicRequest PrivateRequest) t m)
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
