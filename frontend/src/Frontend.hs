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
import Data.Witherable
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

type ExampleCredential = Signed (AuthToken Identity)
type ExampleWidget = RhyoliteWidget
  (AuthMapV ExampleCredential PrivateChatV (Const SelectedCount))
  (ApiRequest ExampleCredential PublicRequest PrivateRequest)

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
          el "div" $ do
            el "h1" $ text "The contents of this section depend on your authentication state"
            pb <- getPostBuild
            fmap switchDyn $ widgetHold (pure never) $ ffor (leftmost [tag (current mAuthCookie) pb, updated mAuthCookie]) $ \case
              Nothing -> do
                el "p" $ text $ "You are not authenticated."
                goToLoginClick <- button "Log In"
                setRoute $ FrontendRoute_Login :/ () <$ goToLoginClick
                pure never
              Just authToken -> do
                fmap switchDyn $ mapRoutedT (authenticatedWidget authToken) $ handleAuthFailure
                  (do
                    el "p" $ text $ "Your token is invalid."
                    pure never
                  )
                  (do
                    el "p" $ text $ "You are authenticated."
                    logoutClick <- button "Log Out"
                    pure $ Nothing <$ logoutClick
                  )
        ) :: forall a. FrontendRoute a -> RoutedT t a (ExampleWidget t m) (Event t (Maybe (Signed (AuthToken Identity)))))
  -- Handle setting the cookies on auth change if we're running in the browser
  prerender_ (pure ()) $ do
    doc <- currentDocumentUnchecked
    performEvent_ $ ffor authChange $ \newAuth -> do
      cookie <- defaultCookieJson authCookieName newAuth
      setPermanentCookie doc cookie
    pure ()
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
  setRoute $ FrontendRoute_Main :/ () <$ catMaybes (leftmost [ tag (current mAuthCookie) pb, updated mAuthCookie ])
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

queryDynE
  :: ( MonadQuery t (ErrorV e v (Const SelectedCount)) m
     , Reflex t
     , EmptyView v
     )
  => Dynamic t (ErrorV e v (Const SelectedCount))
  -> m (Dynamic t (Either e (v Identity)))
queryDynE = fmap (fmap observeErrorV) . queryDyn

authenticatedWidget
  :: ( Ord token, View v
     , Group (v (Const SelectedCount)), Additive (v (Const SelectedCount)), Semigroup (v Identity)
     , QueryResult (v (Const SelectedCount)) ~ v Identity
     , MonadFix m, PostBuild t m
     )
  => token
  -> QueryT t (ErrorV () v (Const SelectedCount)) (RequesterT t (ApiRequest () publicRequest privateRequest) Identity m) a
  -> RhyoliteWidget (AuthMapV token v (Const SelectedCount)) (ApiRequest token publicRequest privateRequest) t m a
authenticatedWidget token = mapAuth token (authMapQueryMorphism token)

handleAuthFailure
  :: ( EmptyView v, PostBuild t m
     , Group (v (Const SelectedCount)), Additive (v (Const SelectedCount)), Semigroup (v Identity)
     , QueryResult (v (Const SelectedCount)) ~ v Identity
     , Adjustable t m, MonadHold t m, MonadFix m
     , Eq (v (Const SelectedCount))
     )
  => RoutedT t r (QueryT t () m) a
  -> RoutedT t r (QueryT t (v (Const SelectedCount)) m) a
  -> RoutedT t r (QueryT t (ErrorV () v (Const SelectedCount)) m) (Dynamic t a)
handleAuthFailure placeholder authenticatedChild = do
  pb <- getPostBuild
  ev <- eitherDyn . fmap observeErrorV =<< askQueryResult
  widgetHold (mapRoutedT (withQueryT unsafeProjectE) placeholder) $ ffor (leftmost [tag (current ev) pb, updated ev]) $ \case
    Left _ -> mapRoutedT (withQueryT unsafeProjectE) placeholder
    Right _ -> mapRoutedT (withQueryT unsafeProjectV) authenticatedChild
