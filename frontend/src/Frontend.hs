{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module Frontend where

import Control.Category
import Control.Lens
import Control.Monad
import Control.Monad.Identity
import Control.Monad.IO.Class
import Control.Monad.Fix
import qualified Data.Aeson as A
import qualified Data.List as L
import Data.Text (Text)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe
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
import qualified GHCJS.DOM.Element as JS
import qualified Language.Javascript.JSaddle as JS
import Data.Vessel
import Rhyolite.Vessel.Path
import Rhyolite.SemiMap
import Data.Map.Monoidal as MMap hiding (keys)
import Common.View

import Common.Request
import Common.Route
import Common.Schema

import Frontend.Authentication
import Frontend.Channel (channel)
import Frontend.Puzzle (puzzles, huntselect)
import Frontend.Types
import Frontend.Utils
-- import OldFrontend

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
      el "title" $ text "Hunt Master List"
      elAttr "link" ( "rel" =: "stylesheet" <> "media" =: "screen" <> "href" =: $(static "css/main.css") <> "type" =: "text/css") $ blank
      elAttr "link" ( "rel" =: "stylesheet" <> "media" =: "screen" <> "href" =: "https://fontlibrary.org/face/symbola" <> "type" =: "text/css") $ blank
      elAttr "link" ( "rel" =: "stylesheet" <> "media" =: "screen" <> "href" =: "https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.4.1/semantic.min.css" <> "integrity"=:"sha512-8bHTC73gkZ7rZ7vpqUQThUDhqcNFyYi2xgDgPDHc+GXVGHXq+xPjynxIopALmOPqzo9JZj0k6OqqewdGO3EsrQ==" <> "crossorigin"=:"anonymous" <> "type" =: "text/css") $ blank
      elAttr "link" ( "rel" =: "stylesheet" <> "media" =: "screen" <> "href" =: "https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.4.1/components/table.min.css" <> "type" =: "text/css") $ blank
      elAttr "link" ( "rel" =: "stylesheet" <> "media" =: "screen" <> "href" =: "https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.4.1/components/popup.min.css" <> "type" =: "text/css") $ blank
      elAttr "link" ( "rel" =: "stylesheet" <> "media" =: "screen" <> "href" =: "https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.4.1/components/icon.min.css" <> "type" =: "text/css") $ blank

      elAttr "link" ("rel" =: "stylesheet" <> "href" =: "https://use.typekit.net/csf8rij.css") blank
      elAttr "link" ("href" =: $(static "css/styles.css") <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
      elAttr "link" ("rel" =: "preconnect" <> "href" =: "https://fonts.googleapis.com") blank
      elAttr "link" ("rel" =: "preconnect" <> "href" =: "https://fonts.gstatic.com" <> "crossorigin" =: "") blank
      elAttr "link" ("rel" =: "stylesheet" <> "href" =: "https://fonts.googleapis.com/css2?family=Karla:ital,wght@0,200;0,300;0,400;0,500;0,600;0,700;0,800;1,200;1,300;1,400;1,500;1,600;1,700;1,800&display=swap") blank
      elAttr "link" ("rel" =: "stylesheet" <> "href" =: "https://fonts.googleapis.com/icon?family=Material+Icons") blank
      elAttr "meta" ("name"=: "viewport" <> "content" =: "width=device-width, initial-scale=1") blank
      elAttr "script" ("src" =: "https://apis.google.com/js/platform.js") blank
      el "script" $ text " \
          \ var prev=5; \
          \ function renderButton() {\
          \   gapi.signin2.render('signin-button', {});\
          \ }"
      blank
  , _frontend_body = runExampleWidget frontendBody
  }

initGauth
  :: ( DomBuilder t m
     , Prerender js t m
     , MonadHold t m
     , PostBuild t m
     , Request m ~ ApiRequest AuthToken PublicRequest PrivateRequest
     , Response m ~ Identity
     , HasConfigs (Client m)
     , Requester t m)
--  => m (Dynamic t (Maybe (Text)))
  => m (Event t AuthToken)
initGauth = do
  elClass "div" "w-screen h-screen bg-background flex flex-col overflow-hidden" $ divClass "p-4 mx-auto md:w-sm t_login" $ do
    elClass "h1" "font-karla font-bold text-h1 text-copy mt-12" $ text "Login"
    elClass "div" "flex flex-col mt-4" $ elAttr "div" ("href" =: "#" <> "id" =: "signinButton") $ text "GOOGLE"
  updatedUserE <- fmap switchDyn $ prerender (return never) $ do
    allowsForcedDevLogin <- getConfig "common/allows_forced_login"
    if isJust allowsForcedDevLogin then (return never) else do
      pb <- getPostBuild
      (updatedUserE :: Event t (Maybe Text), updatedUserT) <- newTriggerEvent
      void $ JS.liftJSM $ do
        updatedUserJS <- JS.toJSVal $ JS.fun $ \ _ _ [ipt] -> JS.eval ("prev.getAuthResponse().id_token"::Text) >>= JS.fromJSVal >>= (liftIO . updatedUserT)
        JS.setProp "fireSigninEvt" updatedUserJS JS.global
        JS.eval $ ("fireSigninEvtInner = (e) => { /*console.log(prev != e); */ console.log(e.getAuthResponse().id_token); /* if(prev != e)*/ { prev = e; fireSigninEvt([String(e.getAuthResponse().id_token)]);} }" :: Text)
      -- let client_id = "570358826294-2ut7bnk6ar7jmqifsef48ljlk0o5m8p4.apps.googleusercontent.com";
      client_id <- fromMaybe "" . (>>= A.decodeStrict') <$> getConfig "common/clientGoogleKey"
      performEvent_ $ (void $ JS.liftJSM $ JS.eval ("console.log('"<>client_id<>"'); gapi.load('auth2', () => {auth2 = gapi.auth2.init({client_id: '" <> client_id <> "'}); auth2.attachClickHandler('signinButton', {}, fireSigninEvtInner, console.log); gapi.signin2.render('signinButton', {onsuccess: fireSigninEvtInner}); if(gapi.auth2.getAuthInstance().isSignedIn) { /*fireSigninEvtInner(gapi.auth2.getAuthInstance().currentUser.get());*/ }}); " :: Text)) <$ pb
      prevState <- hold Nothing $ traceEvent "UpdatedUserE" updatedUserE
      return $ attachWithMaybe (\a b -> if a/=b then Just b else Nothing) prevState updatedUserE

-- This is for debugging, because it's inconvenient to use google auth locally.
  forceLoginEvent :: Event t () <- switchDyn <$> (prerender (pure never) (do
    allowsForcedDevLogin <- getConfig "common/allows_forced_login"
    if isJust allowsForcedDevLogin then text "Yep" >> buttonClass "nope" "Force Login" else pure never
    ))
  let forcedLoginRequest = (ApiRequest_Public PublicRequest_ForceLogin) <$ forceLoginEvent

  let loginRequest = (ApiRequest_Public . PublicRequest_GoogleLogin) <$> fmapMaybe id updatedUserE
  loginResponse <- requestingIdentity $ leftmost [ loginRequest, forcedLoginRequest ]
  let loginSuccess = fmapMaybe (^? _Right) $ traceEvent "Login Response" $ loginResponse
  return $ traceEvent "Updated Login" loginSuccess
  -- tellEvent $ First <$> loginSuccess
  
  --holdDyn Nothing $ updatedUserE
{-
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
-}

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
        FrontendRoute_Puzzle -> authenticateWithToken mAuthCookie $ do
          logout <- puzzles
          pure $ Nothing <$ logout
        FrontendRoute_HuntSelection -> authenticateWithToken mAuthCookie $ do
          logout <- huntselect
          pure $ Nothing <$ logout
        FrontendRoute_Auth -> do
          r <- askRoute
          {-let mode = ffor r $ \case
                AuthRoute_Signup :/ () -> LoginMode_Signup
                AuthRoute_Login :/ () -> LoginMode_Login-}
          -- rec credentials <- do
          token <- initGauth
              
              {-   auth mode $ fmap Just errors
              x :: Dynamic t (Event t (Either Text AuthToken)) <- subRoute $ \case
                AuthRoute_Login ->
                  requestingIdentity . ffor credentials $ \(user, pw) ->
                    ApiRequest_Public $ PublicRequest_Login user pw
                AuthRoute_Signup -> 
                  requestingIdentity . ffor credentials $ \(user, pw) ->
                    ApiRequest_Public $ PublicRequest_Signup user pw
              let (errors, token)  = fanEither $ switch $ current x -}
          authenticateWithToken mAuthCookie $ do
            redirectIfAuthenticated mAuthCookie
            pure $ never
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
     , Requester t m, Response m ~ Identity, Request m ~ ApiRequest () PublicRequest PrivateRequest
     , MonadQuery t (Vessel V (Const SelectedCount)) m
     , MonadHold t m
     , MonadFix m 
     )
  => Dynamic t (Maybe token)
  -> m ()
redirectIfAuthenticated mAuthCookie = do
  pb <- getPostBuild
  let hasAuth = W.catMaybes $ leftmost
        [ tag (current mAuthCookie) pb
        , updated mAuthCookie
        ]
  liveHunts <- fmap (fmap (fromMaybe mempty)) $ watch $ constDyn $ key V_LiveHunts ~> key () ~> postMap (traverse (fmap (Map.keysSet . getMonoidalMap) . getComplete))
  setRoute $ fmap (\hunt -> FrontendRoute_Puzzle :/ (hunt, Nothing)) `fmapMaybe` (current (Set.lookupMax <$> liveHunts) <@ hasAuth)

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

