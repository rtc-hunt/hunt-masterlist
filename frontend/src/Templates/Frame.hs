{-# LANGUAGE OverloadedStrings #-}
module Templates.Frame where

import Reflex.Dom.Core
import Reflex.Dom.Builder.Class(Element(..))
import Reflex
import Control.Monad.Fix
import Data.Text (Text)
import Data.Text as T
import Obelisk.Route.Frontend
import Common.Route
import Control.Monad.Reader
import Data.Functor.Identity
import Common.Schema
import Control.Lens ((^.))
import Language.Javascript.JSaddle
import Frontend.Utils
import GHCJS.DOM.Node (toNode, IsNode)
import Data.Maybe

data TabLayout
  = FullTab
  | SplitTab
  | MutedChat
  deriving (Show, Ord, Eq)

data MenuSettings t = MenuSettings
  { _menuSettings_layout :: Dynamic t TabLayout
  }

data Framed m t a = Framed
  { _framed_headerItems :: m (a)
  , _framed_body :: a -> Event t Text -> Event t Text -> MenuSettings t -> m (Event t Text)
  , _framed_layout :: MenuSettings t -> a -> Dynamic t TabLayout
  , _framed_hunt :: Dynamic t (Id Hunt)
  , _framed_settingspanel :: (Client m) (Event t ())
  }

reloadingRouteLink :: (Monad m, DomBuilder t m, MonadFix m, SetRoute t (R FrontendRoute) m, RouteToUrl (R FrontendRoute) m, PostBuild t m, MonadHold t m, Prerender t m) => Dynamic t (R FrontendRoute) -> m () -> m ()
reloadingRouteLink routeD a = do
    toUrl <- askRouteToUrl
    elDynAttr "a" ((\route -> "href" =: (toUrl $ route)) <$> routeD) $ a

framed :: (Monad m, DomBuilder t m, MonadFix m, SetRoute t (R FrontendRoute) m, RouteToUrl (R FrontendRoute) m, PostBuild t m, MonadHold t m, Prerender t m, MonadReader (Dynamic t (UserSettings Identity)) m, MonadReader (Dynamic t (UserSettings Identity)) (Client m), PerformEvent t m, MonadJSM (Performable (Client m)))
   => Framed m t a -> m ()
framed Framed
  { _framed_headerItems = header
  , _framed_body = body
  , _framed_layout = layout
  , _framed_hunt = huntId
  , _framed_settingspanel = userSettingsPanel
  }
  = mdo
    (menuStuff, a) <- elClass "nav" "app ui fixed inverted menu" $ mdo
      -- toUrl <- askRouteToUrl
      -- dynRouteLink ((\hid -> FrontendRoute_Puzzle :/ (hid, Left mempty)) <$> huntId) $ divClass "logo header item whitespace-nowrap" $ text "Hunt Master List"
      -- elDynAttr "a" ((\hid -> "href" =: (toUrl $ FrontendRoute_Puzzle :/ (hid, Left mempty))) <$> huntId) $ divClass "logo header item whitespace-nowrap" $ text "Hunt Master List"
      reloadingRouteLink ((\hid -> FrontendRoute_Puzzle :/ (hid, Left mempty)) <$> huntId) $ divClass "logo header item whitespace-nowrap" $ text "Hunt Master List"


      rv <- header
      dialogResult <- prerender (pure Nothing) $ fmap (Just) $ elAttr' "dialog" ("id" =: "myModal" <> "closedby" =: "any") $ do
         userSettingsPanel
      let dialogEl = fmap (fmap fst) dialogResult
      prerender blank $
         performEvent_ $ fmapMaybe (fmap $ \dialogEl -> (void $ liftJSM $ (pToJSVal $ toNode $ _element_raw $ dialogEl) ^. js0 ("close"::Text))) $ current dialogEl <@ switch (current $ fromMaybe never . fmap snd <$> dialogResult)
      
      (menuElem, (layoutD, menuOpenD)) <- elClass "div" "right menu" $ elDynAttr' "div" (ffor menuOpenD $ \c -> "class" =: ("ui icon top right dropdown button item " <> c)) $ do
        openToggle <- elClass' "i" "dropdown icon p-4" blank
        isOpen <- toggle False $ () <$ domEvent Click menuElem
        let menuOpenClass = ffor isOpen $ \case
              True -> "active visible"
              False -> ""
        let menuItemOpenClass = ffor isOpen $ \case
              True -> "active transition visible"
              False -> ""
        menuRes <- elDynAttr "div" (ffor menuItemOpenClass $ \c -> "class" =: ("menu " <> c)) $ do
          userSettings <- ask
          switchLayout <- elClass "div" "item" $ semToggleOverride "Show Chat" (_userSettings_chatSidebar <$> userSettings)
          muteChat <- elClass "div" "item" $ semToggleOverride "Mute Chat" (_userSettings_chatMuted <$> userSettings)
 
          (showSettingsBtn, _) <- elClass' "div" "item" $ text "Settings"
          prerender blank $
             performEvent_ $ fmapMaybe (fmap $ \dialogEl -> (void $ liftJSM $ (pToJSVal $ toNode $ _element_raw $ dialogEl) ^. js0 ("showModal"::Text))) $ current dialogEl <@ (domEvent Click showSettingsBtn)
          -- switchLayout <- button "Switch layout" >>= toggle False
          -- muteChat <- button "Mute Chat" >>= toggle False
          let layoutD = ffor ((,) <$> muteChat <*> switchLayout) $ \case
                (False, False) -> FullTab
                (False, True) -> SplitTab
                (True, _) -> MutedChat

          elClass "div" "item" $ routeLink (FrontendRoute_HuntSelection :/ ()) $ elClass "div" "button huntlist-button-color" $ text "Prior Hunts"
          return $ MenuSettings layoutD
        return (menuRes, menuOpenClass)
      return (layoutD, rv)

    divClass "appMain" $ elDynAttr "div" (("class" =:) . T.pack . show <$> layout menuStuff a) $ mdo
      cmdResults <- body a msg cmd menuStuff
      commandOutputWidget cmdResults esc
      (msg, cmd, esc) <- chatInput "Send a message..."
      pure ()
    pure ()


commandOutputWidget cliErrors esc = prerender_ blank $ do
        clearErrors <- fmap switchDyn $ prerender (return never) $ debounce 10 $ "" <$ cliErrors
        lastError <- holdDyn "" $ leftmost [cliErrors, clearErrors, "" <$ esc]
        -- lastError <- holdDyn "" cliErrors
        (el, _) <- elAttr' "pre" ("class" =: "commandOutput") $ dynText lastError
        blank
        -- performEvent_ $ (liftIO $ putStrLn "cliErrors called") <$ cliErrors
        -- performEvent_ $ (void $ liftJSM $ (toNode $ _element_raw el) ^. js0 ("showPopover" :: Text)) <$ cliErrors

chatInput :: (DomBuilder t m, MonadFix m) => Text -> m (Event t Text, Event t Text, Event t ())
chatInput placeholder = do
  rec i <- inputElement $ def
        & initialAttributes .~ ("placeholder" =: placeholder <> "class" =: "chatInput" <> "tabindex" =: "1" <> "popovertarget" =: "cliOutput")
        & inputElementConfig_setValue .~ ("" <$ e)
      let e = leftmost [keypress Enter i]
      let esc = keypress Escape i
  let v  = current $ value i
      enteredE = gate (not . T.null <$> v) $ v <@ e
      chatMessage = ffilter (not . T.isPrefixOf "/") enteredE
      textCmd = fmapMaybe (T.stripPrefix "/") enteredE
  return (chatMessage, textCmd, esc)
