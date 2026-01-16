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
import Data.Either.Combinators
import qualified Data.Aeson as A
import Data.Either
import qualified Data.List as L
import Data.Text (Text)
import qualified Data.Map as Map
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Set as Set
import Data.Maybe
import Control.Monad.Reader
import qualified Data.Text.Encoding as T
import qualified Data.Witherable as W
import GHCJS.DOM (currentDocumentUnchecked, currentDocument)
import GHCJS.DOM.Document (getBody)
import GHCJS.DOM.Node (Node)
import GHCJS.DOM.NodeList (IsNodeList, item, getLength)
import GHCJS.DOM.ParentNode (querySelectorAll)
import Control.Monad.Trans.Maybe
import Data.Signed
import Obelisk.Configs
import Obelisk.Frontend
import Obelisk.Generated.Static
import Obelisk.Route
import Obelisk.Route.Frontend
import Prelude hiding ((.), id)
import Reflex.Dom.Core
import Rhyolite.Api (ApiRequest(..))
import Rhyolite.Frontend.App
import Rhyolite.Frontend.Cookie hiding (getCookie, getCookieJson)
import qualified GHCJS.DOM.Element as JS
import qualified Language.Javascript.JSaddle as JS
import Data.Vessel
import Rhyolite.Vessel.Path
import Rhyolite.SemiMap
import Data.Map.Monoidal as MMap hiding (keys)
import Common.View
import Web.Cookie

import Common.Request
import Common.Route
import Common.Schema
import Data.IORef
import qualified Data.ByteString.Base64 as B64

import Frontend.Authentication
import Frontend.Channel (channel)
import Frontend.Puzzle (puzzles, huntselect)
import Frontend.Types
import Frontend.Utils
import Frontend.ViewCache
import Rhyolite.Frontend.Auth.App
-- import OldFrontend
import Debug.Trace hiding (traceEvent)
import Common.View
import Rhyolite.Vessel.AuthenticatedV

import TemplateViewer
import Templates.Login


{-j
frontend :: Frontend (R FrontendRoute)
frontend = frontendInner $ Just (\q -> traceM "In the ob run query handler" >> return mempty)
-}

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
      --elAttr "script" ("src" =: $(static "jslib.js")) blank
      el "script" $ do
          text "var globalToken; var callback; window.handleGoogleLogin = (e) => { globalToken = e.credential; if( window.callback ){ window.callback(globalToken); } }; window.setCredentialCallback = (cb) => { console.log(\"Calling callback\", cb, globalToken); window.callback = cb; if( globalToken ) { window.callback(globalToken); }; }; "
          text "window.flingMessages = (title, body) => { if( Notification.permission == \"granted\" && window.enableNotifications && document.visibilityState == \"hidden\" ) { new Notification(title, {body, tag: \"HML Chat\"}) } }; "
--j      elAttr "script" ("src" =: "https://apis.google.com/js/platform.js") blank
--      el "script" $ text " \
--          \ var prev=5; \
--          \ function renderButton() {\
--          \   gapi.signin2.render('signin-button', {});\
--          \ }"
      subRoute_ $ \case
        FrontendRoute_Main ->
          elAttr "meta" ("http-equiv" =: "refresh" <> "content" =: ("0; url=" <> "https://hunt-masterlist.tcita.com/hunt/5/" )) blank
        _ -> blank
      blank
  , _frontend_body = runExampleWidget frontendBody
  }

initGauth
  :: ( DomBuilder t m
     , Prerender t m
     , MonadHold t m
     , PostBuild t m
     , Request m ~ ApiRequest AuthToken PublicRequest PrivateRequest
     , Response m ~ Identity
     , HasConfigs m
     , HasConfigs (Client m)
     , TriggerEvent t (Client m)
     , JS.MonadJSM (Client m)
     , Requester t m)
--  => m (Dynamic t (Maybe (Text)))
  => m ()
initGauth = do
  elClass "div" "w-screen h-screen bg-background flex flex-col overflow-hidden" $ divClass "p-4 mx-auto md:w-sm t_login" $ do
    elClass "h1" "font-karla font-bold text-h1 text-copy mt-12" $ text "Login"
    elClass "div" "flex flex-col mt-4" $ do
      client_id <- fromMaybe "" . (>>= A.decodeStrict') <$> getConfig "common/clientGoogleKey"
      elAttr "script" ("src" =: "https://accounts.google.com/gsi/client" <> "async" =: "") blank
      el "div" $ do
        elAttr "div" ("id" =: "g_id_onload" <> "data-auto_prompt" =: "true"
                        <> "data-use_fedcm_for_prompt" =: "true"
                        <> "data-use_fedcm_for_button" =: "true"
                        <> "data-client_id" =: client_id
                        <> {- "data-ux_mode" =: "redirect" <> -} "data-callback" =: "handleGoogleLogin") $ blank -- text "googly appendage"
        elAttr "div" ("class" =: "g_id_signin" <> "data-type" =: "standard" <> "data-size" =: "large" <> "data-theme" =: "outline" <> "data-text" =: "sign_in_with") $ blank

  updatedUserE <- fmap switchDyn $ prerender (return never) $ do
    allowsForcedDevLogin <- getConfig "common/allows_forced_login"
    (updatedUserE :: Event t (Maybe Text), updatedUserT) <- newTriggerEvent
    void $ JS.liftJSM $ do
        updatedUserJS <- JS.toJSVal $ JS.fun $ \ _ _ [ipt] -> JS.fromJSVal ipt >>= (liftIO . updatedUserT)
        JS.jsg1 ("setCredentialCallback" :: Text) updatedUserJS
    prevState <- hold Nothing $ traceEvent "UpdatedUserE" updatedUserE
    -- if isJust allowsForcedDevLogin then (return never) else do
    return $ attachWithMaybe (\a b -> if a/=b then Just b else Nothing) prevState updatedUserE

-- This is for debugging, because it's inconvenient to use google auth locally.
  forceLoginEvent :: Event t () <- switchDyn <$> (prerender (pure never) (do
    allowsForcedDevLogin <- getConfig "common/allows_forced_login"
    if isJust allowsForcedDevLogin then buttonClass "nope" "Force Login" else pure never
    ))
  let forcedLoginRequest = (ApiRequest_Public PublicRequest_ForceLogin) <$ forceLoginEvent

  let loginRequest = (ApiRequest_Public . PublicRequest_GoogleLogin) <$> fmapMaybe id updatedUserE
  loginResponse <- requestingIdentity $ leftmost [ loginRequest, forcedLoginRequest ]
  let loginSuccess = fmapMaybe (^? _Right) $ traceEvent "Login Response" $ loginResponse
  prerender blank $
    performEvent_ $ ffor loginSuccess $ \(token) -> do
      -- traceM $ ("Token serialized as " <>) $ show $ unSigned token
      doc <- currentDocumentUnchecked
      cookie <- defaultCookieJson authCookieName (Just $ token)
      -- traceM $ show cookie
      setPermanentCookie doc (cookie { setCookiePath = Just "/" })
      JS.liftJSM $ do 
         JS.eval ( "console.log(\"Reloading due to login\"); location.reload();" :: Text )
         pure ()
  pure ()
  -- return $ traceEvent "Updated Login" loginSuccess
  -- tellEvent $ First <$> loginSuccess
  
  --holdDyn Nothing $ updatedUserE
{-
auth :: forall js t m.
  ( PostBuild t m
  , MonadHold t m
  , DomBuilder t m
  , RouteToUrl (R FrontendRoute) m
  , SetRoute t (R FrontendRoute) m
  , Prerender t m
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

{-
-- | Handle setting the user's cookie to the given auth token (if any)
manageAuthCookie :: (DomBuilder t m, Prerender t m)
  => Event t (Maybe AuthToken)
  -> m ()
manageAuthCookie authChange = do
  prerender_ blank $ do
    doc <- currentDocumentUnchecked
    performEvent_ $ ffor authChange $ \newAuth -> do
      cookie <- defaultCookieJson authCookieName newAuth
      traceM cookie
      setPermanentCookie doc (cookie { setCookiePath = Just "/" })
-}

-- | Retrieve the value of the given cookie
getCookie :: HasCookies m => Text -> m (Either GetCookieFailed Text)
getCookie key = do
  cookies <- askCookies
  pure $ case Prelude.lookup (T.encodeUtf8 key) $ cookies of
    Nothing -> Left GetCookieFailed_NotFound
    Just c -> join $ mapBoth GetCookieFailed_Base64DecodeFailed (mapLeft (const $ GetCookieFailed_Base64DecodeFailed "utf decode failed") . T.decodeUtf8') $
      base64Decode c

-- | Read a cookie. You may want to use 'Obelisk.Frontend.Cookie.askCookies'
-- along with 'base64Decode' instead.
getCookieJson :: (A.FromJSON v, Monad m, HasCookies m) => Text -> m (Either GetCookieJsonFailed v)
getCookieJson k = do
  r <- fmap (A.eitherDecode . LBS.fromStrict . T.encodeUtf8) <$> getCookie k
  pure $ case r of
    Left failure -> Left $ GetCookieJsonFailed_GetCookieFailed failure
    Right (Left parseFailure) -> Left $ GetCookieJsonFailed_ParseFailure parseFailure
    Right (Right v) -> Right v


frontendBody
  :: forall js t m.
     ( ObeliskWidget t (R FrontendRoute) m
     )
  => RoutedT t (R FrontendRoute) (FullAppWidget MasterlistApp t m) ()
frontendBody = do
  cookies <- askCookies
  -- display $ constDyn cookies
  subRoute_ $ \case
    FrontendRoute_Main -> void $ prerender blank $ do
      JS.liftJSM $ do 
         JS.eval ( "location.assign(\"/hunt/5/\");" :: Text )
         pure ()
    _ -> blank

  -- let mAuthCookie0 = A.decodeStrict . (\t -> if BS.head t == 34 then t else (34 `cons` t) `snoc` 34) =<< L.lookup (T.encodeUtf8 authCookieName) cookies
  -- let mAuthCookie0 = L.lookup (T.encodeUtf8 authCookieName) cookies >>= ((\case {Left _ -> Nothing; Right a -> Just a; }) . B64.decode) >>= A.decodeStrict
  -- let mAuthCookie0 = getCookieJson 
  mAuthCookie0 <- getCookieJson (authCookieName)
  let clearCookie = prerender blank $ do
        doc <- currentDocumentUnchecked
        cookie <- defaultCookie authCookieName Nothing
        setPermanentCookie doc (cookie { setCookiePath = Just "/" })
        JS.liftJSM $ do 
           JS.eval ( "console.log(\"Reloading due to login\"); location.reload();" :: Text )
           pure ()

  -- display $ constDyn mAuthCookie0
  case mAuthCookie0 of
    Left _ -> subRoute_ $ \case
        FrontendRoute_Main -> blank
        _ -> void $ initGauth
    Right token -> do
      traceM $ ("TOKEN: " <>) $ show $ token
      -- display $ constDyn token
      void $ mapRoutedT (authenticatedWidget (Proxy :: Proxy MasterlistApp) token) $ handleAuthFailure (void (renderInvalid >> clearCookie)) $ do
        userSettings <- fmap (fmap $ fromMaybe def) $ watch $ constDyn $ personalP ~> key PV_Settings ~> singleV ~> postMap (\a -> Just $ Identity $ fromMaybe def $ runIdentity a)
        mapRoutedT (flip runReaderT userSettings) $ subRoute_ $ \case
          FrontendRoute_Templates -> void $ templateViewer
          FrontendRoute_Channel -> void $ channel
          FrontendRoute_Puzzle -> void $ puzzles
          FrontendRoute_HuntSelection -> void $ huntselect
          FrontendRoute_Main -> blank
 {-
-- do
  -- display $ constDyn mAuthCookie0
  -- text $ Data.Text.pack $ show mAuthCookie0
  -- sample (current mAuthCookie0) >>= text
  -- rec mAuthCookie <- holdDyn mAuthCookie0 authChange
  -- We have to give a type signature to the argument of subRoute because
  -- (at least on GHC 8.6.5) the compiler cannot properly infer that
  -- this function has the proper higher rank type.
      subRoute $ ((\case
        FrontendRoute_Templates -> templateViewer >> return never
        FrontendRoute_Channel -> do
          logout <- channel
          pure $ Nothing <$ logout
        FrontendRoute_Puzzle ->  do
          logout <- puzzles
          pure $ Nothing <$ logout
        FrontendRoute_HuntSelection -> do
          logout <- huntselect
          pure $ Nothing <$ logout
        FrontendRoute_Auth -> do
          r <- askRoute
          {-let mode = ffor r $ \case
                AuthRoute_Signup :/ () -> LoginMode_Signup
                AuthRoute_Login :/ () -> LoginMode_Login-}
          -- rec credentials <- do
          -- token <- initGauth
              
              {-   auth mode $ fmap Just errors
              x :: Dynamic t (Event t (Either Text AuthToken)) <- subRoute $ \case
                AuthRoute_Login ->
                  requestingIdentity . ffor credentials $ \(user, pw) ->
                    ApiRequest_Public $ PublicRequest_Login user pw
                AuthRoute_Signup -> 
                  requestingIdentity . ffor credentials $ \(user, pw) ->
                    ApiRequest_Public $ PublicRequest_Signup user pw
              let (errors, token)  = fanEither $ switch $ current x -}
          -- authenticateWithToken mAuthCookie $ do
          --   redirectIfAuthenticated mAuthCookie
          --  pure $ never
          -- pure $ Just <$> token
          pure never
        FrontendRoute_Main -> do
          pb <- getPostBuild
          setRoute $ FrontendRoute_Auth :/ AuthRoute_Login :/ () <$ pb
          pure never
        )) -- :: forall a. FrontendRoute a -> RoutedT t a (ExampleWidget t m) (Event t (Maybe AuthToken)))
      pure never
  -- manageAuthCookie authChange
  pure ()
  -}

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
  -- setRoute $ fmap (\hunt -> FrontendRoute_Puzzle :/ (hunt, Left mempty)) $ (fromMaybe (HuntId 4) <$> current (Set.lookupMax <$> liveHunts) <@ hasAuth)
  pure ()

runExampleWidget
  :: ( DomBuilder t m
     , HasConfigs m
     , TriggerEvent t m
     , PerformEvent t m
     , MonadHold t m
     , PostBuild t m
     , MonadFix m
     , Prerender t m
     , MonadIO (Performable m)
     , Adjustable t m
     , A.ToJSON MyQueryResultType
     -- , A.ToJSON MyQueryType
     )
  => RoutedT t
      (R FrontendRoute)
      (FullAppWidget MasterlistApp t m)
      a
  -- ^ Child widget
  -> RoutedT t (R FrontendRoute) m a
runExampleWidget = fmap snd . runObeliskRhyoliteWidget (\a -> do
  let queryAction av = do
        let avKey = A.encode $ _queryMorphism_mapQuery vesselToWire av
        handler <- liftIO $ readIORef Common.View.globalMagicQueryHandler
        case handler of
          Nothing -> do
              getCachedViews >>= \case
                Just cache -> do
                    let v = (Map.lookup (show avKey) cache)
                    traceM "Retrieved data from cache"
                    pure (Nothing, fromMaybe mempty v)
                Nothing -> pure (Nothing, mempty)
          Just handler -> do
              traceM "Running the handler"
              rv <- liftIO $ handler av
              traceM $ "Handler ran"
              return (Just (show avKey), rv)
  lastQuery <- holdDyn mempty a
  nubbedLastQuery <- holdUniqDyn lastQuery
  values <- performEvent $ queryAction . (Data.Vessel.mapV (const (Const 1))) <$> updated nubbedLastQuery
  lastValue <- holdDyn mempty values
  nubbedValueUpdates <- holdUniqDyn lastValue
  stored <- foldDyn (<>) mempty $ flip fmapMaybe values (\case
              (Nothing, _) -> Nothing
              (Just key, v) -> Just $ Map.singleton key v
              )
  elAttr "script" ("data-hydration-skip" =: "" <> "data-rhyolite-saved-views" =: "" <> "data-ssr" =: "" <> "type" =: "text/plain") $ dynText $ T.decodeUtf8 . B64.encode . LBS.toStrict . A.encode <$> stored
  performEvent_ $ traceM "nubbedValueUpdates updated" <$ updated nubbedValueUpdates
  pure $ snd <$> updated nubbedValueUpdates
  )

--(fromMaybe (\a -> pure mempty) $ fmap (liftIO .) $ liftIO $ readIORef Common.View.globalMagicQueryHandler)--pure mempty) -- (liftIO . fromMaybe (\q -> traceM "In the magic query handler" >> return mempty) magicQueryHandler)
  vesselToWire
  "common/route"
  checkedFullRouteEncoder
  (BackendRoute_Listen :/ ())
