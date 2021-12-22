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
import Data.Functor.Const
import Data.Functor.Identity
import qualified Data.List as L
import Data.Maybe (isNothing, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Vessel.Class
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
import Rhyolite.Vessel.ErrorV
import Rhyolite.Vessel.AuthMapV

import Common.Request
import Common.Route
import Common.View

import Frontend.Authentication

import Templates.Partials.Containers
import Templates.Partials.ChannelList
import Templates.Partials.TextInput
import Templates.Partials.PasswordInput
import Templates.Partials.Buttons
import Templates.Partials.Headers
import Templates.Channel

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
      el "title" $ text "Rhyolite Example"
      elAttr "link" ("rel" =: "stylesheet" <> "href" =: "https://use.typekit.net/csf8rij.css") blank
      elAttr "link" ("href" =: $(static "styles.css") <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
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
    link (FrontendRoute_SignUp :/ ()) "Don't have an account?"
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

signUp :: forall js t m. (MonadFix m, MonadHold t m, PostBuild t m, DomBuilder t m, RouteToUrl (R FrontendRoute) m, SetRoute t (R FrontendRoute) m, Prerender js t m) => m (Event t (Text, Text))
signUp = screenContainer $ do
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

-- Surrounds view with header in a screen sized div, default for most pages
appPage :: (DomBuilder t m)
  => m a
  -> m (a, Event t Logout)
appPage action = screenContainer $ do
  logout <- header
  a <- action
  pure (a, logout)

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
        FrontendRoute_Channel -> authenticateWithToken mAuthCookie $ do
          (_, logout) <- appPage $ do
            mcid <- maybeDyn =<< askRoute
            dyn_ . ffor mcid $ \case
              Nothing -> do
                rec cl <- channelList (ChannelListConfig (channelSearchResults cl))
                pure ()
              Just cid -> do
                channelView <- channelBuilder cid
                prerender_ blank $ do
                  rec ChannelOut input send scrollEl <- channelPage $ ChannelConfig
                        { _channelConfig_name = fromMaybe "" <$> _channelView_name channelView
                        , _channelConfig_clearInput = clickOrEnter
                        , _channelConfig_messagesConfig = MessagesConfig $ do
                            dyn_ $ ffor (_channelView_messages channelView) $ \case
                              Nothing -> text "loading..."
                              Just msgs -> messagesHelper scrollEl msgs
                        }
                      let newMessage = current $ value input
                          enter = keypress Enter $ _inputElement_element input
                          clickOrEnter = leftmost [send, enter]
                          sendMessage = gate (not . T.null . T.strip <$> newMessage) clickOrEnter
                      _ <- requestingIdentity $ attachWith
                        (\(c, m) _ -> ApiRequest_Private () $ PrivateRequest_SendMessage c m)
                        ((,) <$> current cid <*> newMessage)
                        sendMessage
                  pure ()
          pure $ Nothing <$ logout
        FrontendRoute_SignUp -> do
          credentials <- signUp
          redirectIfAuthenticated mAuthCookie $ do
            (_loginFailed, loginSuccess) <- fmap fanEither . requestingIdentity . ffor credentials $ \(user, pw) ->
              ApiRequest_Public $ PublicRequest_SignUp user pw
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

queryDynE
  :: ( MonadQuery t (ErrorV e v (Const SelectedCount)) m
     , Reflex t
     , EmptyView v
     )
  => Dynamic t (ErrorV e v (Const SelectedCount))
  -> m (Dynamic t (Either e (v Identity)))
queryDynE = fmap (fmap observeErrorV) . queryDyn
