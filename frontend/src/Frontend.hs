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
import Rhyolite.Vessel.ErrorV
import Rhyolite.Vessel.AuthMapV

import Reflex.Dom.Core hiding (link)

import Common.Api
import Common.Route
import Common.View

import Data.Bool (bool)

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
    when showMenu $ secondaryIconButton "" "menu"
  where
    classes =
      classList [ "font-karla font-bold text-copy bg-white shadow-header flex flex-row justify-between items-center z-10"
                , bool "px-4 py-5" "px-4 py-2" showMenu
                ]

input :: DomBuilder t m => T.Text -> m ()
input label = do
  elClass "div" "flex flex-col mt-8" $ do
    elClass "div" "font-facit font-label text-label" $ text label
    _ <- inputElement $ def
      & initialAttributes .~ ("class" =: "font-facit font-label text-label placeholder-light p-4 bg-inset rounded shadow-input"
                             <> "placeholder" =: label
                             <> "type" =: "text"
                             )
    pure ()

classList :: [T.Text] -> T.Text
classList =
  T.intercalate " "

h1 :: DomBuilder t m => T.Text -> T.Text -> m ()
h1 cs = elClass "h1" (classList ["font-karla font-bold text-h1 text-copy", cs]) . text

passwordInput :: DomBuilder t m => m ()
passwordInput =
  elClass "div" "flex flex-col mt-8" $ do
    elClass "div" "font-facit font-label text-label" $ text "Passphrase"
    elClass "div" "bg-inset rounded shadow-input flex flex-row items-center overflow-hidden" $ do
      _ <- inputElement $ def
        & initialAttributes .~ ("class" =: "flex-grow w-full h-full font-facit font-label text-label bg-transparent placeholder-light p-4"
                               <> "placeholder" =: "Passphrase"
                               <> "type" =: "password"
                               )
      elClass "button" "font-icon text-icon text-primary-darker px-4" $ text "visibility"
      pure ()

cta :: DomBuilder t m => T.Text -> m ()
cta =
  elClass "button" "w-full p-4 mt-16 shadow-button bg-primary font-facit font-bold text-white text-body text-center rounded" . text

link :: (DomBuilder t m, RouteToUrl route m, SetRoute t route m, Prerender js t m) => route -> T.Text -> m ()
link route label = do
  routeLink route $ elClass "div" "font-facit font-label underline text-label text-link text-center mt-4" $ text label

logIn :: (DomBuilder t m, RouteToUrl (R FrontendRoute) m, SetRoute t (R FrontendRoute) m, Prerender js t m) => m ()
logIn = elClass "div" "w-screen h-screen bg-background" $ do
  header False
  elClass "div" "p-4" $ do
    h1 "mt-12" "Log In"

    input "Email/Profile Name"
    passwordInput

    cta "Log In"
    link (FrontendRoute_SignUp :/ ()) "Don't have an account?"
  pure ()

signUp :: (DomBuilder t m, RouteToUrl (R FrontendRoute) m, SetRoute t (R FrontendRoute) m, Prerender js t m) => m ()
signUp = elClass "div" "w-screen h-screen bg-background" $ do
  header False
  elClass "div" "p-4" $ do
    h1 "mt-12" "Sign Up"

    input "Email"
    input "Profile Name"
    passwordInput

    cta "Sign Up"
    link (FrontendRoute_Login :/ ()) "Already have an account?"

widgets :: (DomBuilder t m, RouteToUrl (R FrontendRoute) m, SetRoute t (R FrontendRoute) m, Prerender js t m) => m ()
widgets =
  signUp

listItem :: DomBuilder t m => T.Text -> Maybe T.Text -> m ()
listItem label mSubtext = do
  elClass "div" "flex flex-col py-2 border-b border-metaline" $ do
    elClass "div" "leading-none font-facit text-body text-copy" $ text label
    case mSubtext of
      Just subtext -> elClass "div" "mt-1 leading-none font-facit text-label text-light" $ text subtext
      Nothing -> blank

-- TODO(skylar): Is this just a link?
channelItem :: DomBuilder t m => m ()
channelItem = listItem "#ChannelName" $ Just "543 members \x00b7 13 online"

searchbar :: DomBuilder t m => m ()
searchbar = do
  elClass "div" "mt-2 w-full shadow-button bg-white rounded flex flex-row" $ do
    elClass "button" "font-icon text-icon text-light pl-2" $ text "search"
    _ <- inputElement $ def
      & initialAttributes .~ ("class" =: "flex-grow w-full h-full font-facit font-label text-label bg-transparent placeholder-light pl-1 pt-3 pb-3 pr-3"
                             <> "placeholder" =: "Search for a channel"
                             <> "type" =: "text"
                             )
    pure ()

iconCta :: DomBuilder t m => T.Text -> m ()
iconCta icon = do
  elClass "button" "flex-shrink-0 bg-primary rounded p-2.5 font-icon text-icon text-white leading-none shadow-button" $ text icon

secondaryIconButton :: DomBuilder t m => T.Text -> T.Text -> m ()
secondaryIconButton cs icon = do
  elClass "button" (classList ["rounded border border-metaline p-2.5 flex-shrink-0 bg-primary-light", cs]) $
    elClass "div" "font-icon leading-none text-icon text-primary-dark" $ text icon

secondaryButton :: DomBuilder t m => T.Text -> m ()
secondaryButton label = do
  elClass "button" "w-full mt-4 p-2.5 leading-none text-center rounded border border-metaline bg-primary-light text-primary-darker font-bold font-facit" $
    text label

channelMembers :: DomBuilder t m => m ()
channelMembers = elClass "div" "w-screen h-screen bg-background flex flex-col" $ do
  header True
  elClass "div" "p-4 bg-raised flex flex-row items-center border-b border-metaline" $ do
    secondaryIconButton "" "arrow_back"
    elClass "div" "ml-4 flex flex-col" $ do
      elClass "div" "font-karla font-bold text-h2 text-copy leading-none" $ text "Members"
      elClass "div" "mt-1 leading-none font-facit text-label text-light" $ do
        text "#Channel Name"

  elClass "div" "mt-8 p-4" $ do
    elClass "div" "font-facit text-h2 text-copy" $ text "Online"
    replicateM_ 4 $ listItem "Yasuke" Nothing

    elClass "div" "font-facit text-h2 text-copy mt-6" $ text "Offline"
    replicateM_ 4 $ listItem "Yasuke" Nothing

settings :: DomBuilder t m => m ()
settings = elClass "div" "w-screen h-screen bg-background flex flex-col" $ do
  header True
  elClass "div" "p-4 bg-raised flex flex-row justify-between items-center border-b border-metaline" $ do
    blank

channel :: DomBuilder t m => m ()
channel = elClass "div" "w-screen h-screen bg-background flex flex-col" $ do
  header True
  elClass "div" "p-4 bg-raised flex flex-row justify-between items-center border-b border-metaline" $ do
    elClass "div" "flex flex-col" $ do
      elClass "div" "font-karla font-bold text-h2 text-copy leading-none" $ text "#Channel Name"
      elClass "div" "mt-1 leading-none font-facit text-label text-light" $ do
        text "543 members \x00b7 13 online"

    elClass "div" "flex flex-row items-center" $ do
      secondaryIconButton "" "search"
      secondaryIconButton "ml-2" "more_horiz"

  elClass "div" "flex-grow flex flex-col p-4" $ do
    elClass "div" "font-facit text-copy flex flex-col" $ do
      elClass "div" "flex flex-row items-baseline justify-between" $ do
        elClass "div" "text-label" $ text "Tanko"
        elClass "div" "font-bold text-label text-light" $ text "2:00am"
      elClass "div" "p-4 rounded border border-metaline bg-white" $ text "What is good?"

  elClass "div" "p-1 bg-white flex flex-row justify-center" $ do
    secondaryIconButton "" "add"
    _ <- inputElement $ def
      & initialAttributes .~ ( "class" =: "mx-1 font-facit font-label text-label placeholder-light px-3.5 bg-inset rounded shadow-input flex-grow"
                               <> "placeholder" =: "Type your message"
                               <> "type" =: "text"
                             )
    iconCta "send"

channels :: (DomBuilder t m, RouteToUrl (R FrontendRoute) m, SetRoute t (R FrontendRoute) m, Prerender js t m) => m ()
channels = elClass "div" "w-screen h-screen bg-background flex flex-col" $ do
  header True
  elClass "div" "flex-grow p-4" $ do
    elClass "div" "w-full flex flex-row mt-10 items-center justify-between" $ do
      h1 "" "Channels"
      iconCta "add"
    searchbar

    elClass "div" "mt-8 font-facit text-h2 text-copy" $ text "Recent channels"

    replicateM_ 4 channelItem

  elClass "div" "w-full p-4 border-t border-metaline bg-white" $ do
    elClass "div" "w-full flex flex-row items-center" $ do
      elClass "div" "w-12 h-12 rounded-full bg-primary-darker mr-2 flex-shrink-0" blank
      elClass "div" "flex flex-col font-facit text-label h-full flex-grow" $ do
        elClass "div" "text-copy" $ text "Sky"
        elClass "div" "text-light" $ text "soquinn@obsidian.systems"

      secondaryIconButton "" "settings"
    secondaryButton "Logout"

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
        FrontendRoute_ChannelMembers -> do
          channelMembers
          pure never
        FrontendRoute_Channels -> do
          channels
          pure never
        FrontendRoute_Channel -> do
          channel
          pure never
        FrontendRoute_Widgets -> do
          widgets
          pure never
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
