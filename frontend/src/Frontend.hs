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
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Vessel.Class
import Data.Witherable as W
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
import Rhyolite.Vessel.ErrorV
import Rhyolite.Vessel.AuthMapV

import Reflex.Dom.Core hiding ( link
                              , textInput
                              , TextInputConfig
                              , TextInput(..)
                              , textInput_value
                              , textInput_input
                              , _textInput_value
                              )

import Common.Api
import Common.Route
import Common.View

import Data.Bool (bool)

import Frontend.Utils
import Frontend.Templates.Partials.Switch
import Frontend.Templates.Partials.TextInput
import Frontend.Templates.Partials.PasswordInput
import Frontend.Templates.Partials.SegmentedButton
import Frontend.Templates.Partials.ChannelList
import Frontend.Templates.Partials.Buttons
import Frontend.Templates.Partials.Headers
import Frontend.Templates.Partials.Lists
import Frontend.Templates.Partials.Searchbar

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

header :: DomBuilder t m => Bool -> m ()
header showMenu = do
  elClass "div" classes $ do
    elClass "div" "" $ text "RhyoliteExample"
    when showMenu $ secondaryIconButton "" "menu" >> pure ()
  where
    classes =
      classList [ "font-karla font-bold text-copy bg-white shadow-header flex flex-row justify-between items-center z-10"
                , bool "px-4 py-5" "px-4 py-2" showMenu
                ]

link :: (DomBuilder t m, RouteToUrl route m, SetRoute t route m, Prerender js t m) => route -> T.Text -> m ()
link route label = do
  routeLink route $ elClass "div" "font-facit font-label underline text-label text-link text-center mt-4" $ text label

logIn
  :: forall js t m. (PostBuild t m, MonadFix m, MonadHold t m, DomBuilder t m, RouteToUrl (R FrontendRoute) m, SetRoute t (R FrontendRoute) m, Prerender js t m)
  => Event t (Maybe Text)
  -> m (Event t (Text, Text))
logIn serverError = elClass "div" "w-screen h-screen bg-background" $ do
  header False
  elClass "div" "p-4 mx-auto md:w-sm" $ mdo
    h1 $ def
      & headerConfig_header .~ "Log In"
      & headerConfig_classes .~ "mt-12"

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
signUp = elClass "div" "w-screen h-screen bg-background" $ do
  header False
  elClass "div" "p-4 mx-auto md:w-sm" $ do
    h1 $ def
      & headerConfig_header .~ "Sign Up"
      & headerConfig_classes .~ "mt-12"

    ti <- textInput $ def
      & textInputConfig_label .~ "Email"
      & textInputConfig_type .~ "email"

    textInput $ def
      & textInputConfig_label .~ "Profile Name"

    pi <- passwordInput def

    let
      usernameEvent = _textInput_input ti
      passwordEvent = _passwordInput_input pi

    click <- primaryButton "Sign Up"
    link (FrontendRoute_Login :/ ()) "Already have an account?"
    credentials <- zipDyn <$> holdDyn "" usernameEvent <*> holdDyn "" passwordEvent
    pure $ W.filter testCredentials $ tagPromptlyDyn credentials click

channelSearch :: (MonadHold t m, PostBuild t m, DomBuilder t m, MonadFix m) => m ()
channelSearch = elClass "div" "w-screen h-screen bg-background flex flex-col" $ do
  header True
  elClass "div" "p-4 bg-raised flex flex-row items-center border-b border-metaline" $ do
    secondaryIconButton "" "arrow_back"
    elClass "div" "ml-4 flex flex-col" $ do
      elClass "div" "font-karla font-bold text-h2 text-copy leading-none" $ text "Message Search"
      elClass "div" "mt-1 leading-none font-facit text-label text-light" $ do
        text "#Channel Name"

  elClass "div" "p-4" $ do
    searchbar "Search messages in #Channel Name"
    elClass "div" "flex flex-row mb-4" $ segmentedButton $ def
      & segmentedButtonConfig_segments .~ ["New", "Old", "Relevant"]
      & segmentedButtonConfig_classes .~ "mt-4"

    replicateM_ 4 messageFullWidth

channelMembers :: DomBuilder t m => m ()
channelMembers = elClass "div" "w-screen h-screen bg-background flex flex-col" $ do
  header True
  elClass "div" "p-4 bg-raised flex flex-row items-center border-b border-metaline mb-8" $ do
    secondaryIconButton "" "arrow_back"
    elClass "div" "ml-4 flex flex-col" $ do
      elClass "div" "font-karla font-bold text-h2 text-copy leading-none" $ text "Members"
      elClass "div" "mt-1 leading-none font-facit text-label text-light" $ do
        text "#Channel Name"

  channelMembersWidget

channelMembersWidget :: DomBuilder t m => m ()
channelMembersWidget = do
  elClass "div" "p-4" $ do
    elClass "div" "font-facit text-h2 text-copy" $ text "Online"
    replicateM_ 4 $ listItem "Yasuke" Nothing

    elClass "div" "font-facit text-h2 text-copy mt-6" $ text "Offline"
    replicateM_ 4 $ listItem "Yasuke" Nothing

settings :: forall t m. (DomBuilder t m, PostBuild t m, MonadHold t m) => m ()
settings = elClass "div" "w-screen h-screen bg-background flex flex-col" $ do
  header True
  elClass "div" "p-4 bg-raised flex flex-row items-center border-b border-metaline" $ do
    secondaryIconButton "" "arrow_back"
    elClass "div" "ml-4 flex flex-col" $ do
      elClass "div" "font-karla font-bold text-h2 text-copy leading-none" $ text "Settings"

  elClass "div" "flex flex-col flex-grow p-4" $ do
    elClass "div" "flex justify-center items-center" $ elClass "div" "mt-8 w-32 h-32 rounded-full bg-black" blank

    textInput $ def & textInputConfig_label .~ "Account Name"
    textInput $ def & textInputConfig_label .~ "Email"

    switchButton $ (def :: SwitchConfig t)
      & switchConfig_label .~ "Enable Push Notifications"
      & switchConfig_icon .~ Just "notifications_none"
      & switchConfig_classes .~ "mt-8"
      & switchConfig_disabled .~ pure True

    void $ secondaryButton "mt-8" "Reset your password"

messageFullWidth :: DomBuilder t m => m ()
messageFullWidth = do
  elClass "div" "font-facit text-copy flex flex-col mt-4" $ do
    elClass "div" "flex flex-row items-baseline justify-between" $ do
      elClass "div" "text-label" $ text "Tanko"
      elClass "div" "font-bold text-label text-light" $ text "2:00am"
    elClass "div" "p-4 rounded border border-metaline bg-white" $ text "What is good?"

data MessageConfig t = MessageConfig
   { _messageConfig_viewer :: Dynamic t Bool
   }

message :: (PostBuild t m, DomBuilder t m) => MessageConfig t -> m ()
message (MessageConfig dViewer) = do
  elDynClass "div" (mkClasses <$> dViewer) $ do
    elClass "div" "flex flex-row items-baseline justify-between" $ do
      elClass "div" "text-label md:mr-4" $ text "Tanko"
      elClass "div" "font-bold text-label text-light" $ text "2:00am"
    elClass "div" "p-4 rounded border border-metaline bg-white w-auto" $ text "What is good?"

  where
    mkClasses b =
      classList [ "font-facit text-copy flex flex-col mb-6"
                , bool "mr-16 md:mr-0 md:items-start" "ml-16 md:ml-0 md:items-end" b
                ]

channel :: forall t m js. (PostBuild t m, DomBuilder t m, MonadHold t m, MonadFix m, SetRoute t (R FrontendRoute) m, RouteToUrl (R FrontendRoute) m, Prerender js t m) => m (Event t ())
channel = elClass "div" "w-screen h-screen bg-background flex flex-col" $ do
  header True
  elClass "div" "w-full flex flex-row flex-grow" $ do
    click <- elClass "div" "flex-shrink-0 w-1/4 h-full flex-col bg-sunken hidden md:flex border-r border-metaline" $
      channelList $ def & channelListConfig_useH2 .~ True
    channelInterior
    elClass "div" "flex-shrink-0 w-1/5 bg-sunken hidden md:flex" $ do
      channelMembersWidget
    pure click

channelInterior :: forall t m js. (PostBuild t m, DomBuilder t m, MonadHold t m, MonadFix m, SetRoute t (R FrontendRoute) m, RouteToUrl (R FrontendRoute) m, Prerender js t m) => m ()
channelInterior = elClass "div" "w-full flex flex-col" $ do
  elClass "div" "p-4 bg-raised flex flex-row justify-between items-center border-b border-metaline relative" $ do
    elClass "div" "flex flex-col" $ do
      elClass "div" "font-karla font-bold text-h2 md:text-h1 text-copy leading-none" $ text "#Channel Name"
      elClass "div" "mt-1 leading-none font-facit text-label text-light" $ do
        text "543 members \x00b7 13 online"

    elClass "div" "flex flex-row items-center" $ do
      secondaryIconButton "" "search"
      elClass "div" "" $ do
        eClickMore <- secondaryIconButton "ml-2 md:hidden" "more_horiz"

        dMoreOpen <- foldDyn ($) False $ not <$ eClickMore
        let
          mkMenuClasses b =
            classList [ "absolute left-0 right-0 top-full p-4 md:hidden"
                      , "tranform transition-all duration-150"
                      , bool "pointer-events-none opacity-0" "pointer-events-auto opacity-100" b
                      ]

        elDynClass "div" (mkMenuClasses <$> dMoreOpen) $ do
          elClass "div" "bg-white rounded p-4 shadow-button z-10" $ do
            switchButton $ (def :: SwitchConfig t)
              & switchConfig_label .~ "Notifications"
              & switchConfig_icon .~ Just "notifications_none"
              & switchConfig_disabled .~ pure True

            elClass "div" "flex flex-row items-center mt-4" $ do
              elClass "div" "font-icon leading-none text-icon text-copy mr-1 " $ text "person_outline"
              elClass "div" "font-facit text-body text-copy" $ text "Members"

  elClass "div" "flex-grow flex flex-col p-4" $ do
    message $ MessageConfig $ pure False
    message $ MessageConfig $ pure False
    message $ MessageConfig $ pure True

  elClass "div" "p-1 bg-white flex flex-row justify-center border-t border-metaline" $ do
    secondaryIconButton "" "add"
    _ <- inputElement $ def
      & initialAttributes .~ ( "class" =: "focus:outline-none mx-1 font-facit font-label text-label placeholder-light px-3.5 bg-inset rounded shadow-input flex-grow"
                               <> "placeholder" =: "Type your message"
                               <> "type" =: "text"
                             )
    iconButton "send"

-- Surrounds view with header in a screen sized div, default for most pages
appPage :: DomBuilder t m => m a -> m a
appPage action = elClass "div" "w-screen h-screen bg-background flex flex-col" $ do
  header True
  action

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
        FrontendRoute_Settings -> do
          settings
          pure never
        FrontendRoute_ChannelSearch -> do
          channelSearch
          pure never
        FrontendRoute_ChannelMembers -> do
          channelMembers
          pure never
        FrontendRoute_Channels -> do
          appPage $ channelList $ def
            & channelListConfig_headerClasses .~ "mt-6"
          pure never
        FrontendRoute_Channel -> do
          click <- channel
          setRoute $ FrontendRoute_Login :/ () <$ click
          pure $ Nothing <$ click
        FrontendRoute_SignUp -> do
          credentials <- signUp
          redirectIfAuthenticated mAuthCookie $ do
            (_loginFailed, loginSuccess) <- fmap fanEither . requestingIdentity . ffor credentials $ \(user, pw) ->
              ApiRequest_Public $ PublicRequest_SignUp user pw
            pure (fmap Just loginSuccess)
          {- redirectIfAuthenticated mAuthCookie $ do
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
          pure (fmap Just signupSuccess)-}
        FrontendRoute_Login -> mdo
          credentials <- logIn loginError
          (loginError, cookie) <- redirectIfAuthenticated mAuthCookie $ do
            (loginFailed, loginSuccess) <- fmap fanEither . requestingIdentity . ffor credentials $ \(user, pw) ->
              ApiRequest_Public $ PublicRequest_Login user pw
            pure (fmap Just loginFailed, fmap Just loginSuccess)
          pure cookie
          {- redirectIfAuthenticated mAuthCookie $ do
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
          pure (fmap Just loginSuccess)-}
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
  setRoute $ FrontendRoute_Channel :/ () <$ catMaybes (leftmost [ tag (current mAuthCookie) pb, updated mAuthCookie ])
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
