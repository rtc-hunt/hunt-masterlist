module Templates.Frame where

import Reflex.Dom.Core
import Reflex
import Control.Monad.Fix
import Data.Text (Text)
import Data.Text as T
import Obelisk.Route.Frontend
import Common.Route
import Common.Schema
import Frontend.Utils

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
  , _framed_body :: a -> Event t Text -> Event t Text -> MenuSettings t -> m ()
  , _framed_layout :: MenuSettings t -> a -> Dynamic t TabLayout
  , _framed_hunt :: Dynamic t (Id Hunt)
  }

framed :: (Monad m, DomBuilder t m, MonadFix m, SetRoute t (R FrontendRoute) m, RouteToUrl (R FrontendRoute) m, PostBuild t m, MonadHold t m, Prerender js t m) => Framed m t a -> m ()
framed Framed
  { _framed_headerItems = header
  , _framed_body = body
  , _framed_layout = layout
  , _framed_hunt = huntId
  }
  = mdo
    (menuStuff, a) <- elClass "nav" "app ui fixed inverted menu" $ mdo
      dynRouteLink ((\hid -> FrontendRoute_Puzzle :/ (hid, Left mempty)) <$> huntId) $ divClass "logo header item whitespace-nowrap" $ text "Hunt Master List"

      rv <- header
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
          switchLayout <- elClass "div" "item" $ semToggle "Show Chat" False
          muteChat <- elClass "div" "item" $ semToggle "Mute Chat" False
          divClass "item" $ text "Settings"
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
      body a msg cmd menuStuff
      (msg, cmd) <- chatInput "Send a message..."
      pure ()
    pure ()

chatInput :: (DomBuilder t m, MonadFix m) => Text -> m (Event t Text, Event t Text)
chatInput placeholder = do
  rec i <- inputElement $ def
        & initialAttributes .~ ("placeholder" =: placeholder <> "class" =: "chatInput" <> "tabindex" =: "1")
        & inputElementConfig_setValue .~ ("" <$ e)
      let e = leftmost [keypress Enter i]
  let v  = current $ value i
      enteredE = gate (not . T.null <$> v) $ v <@ e
      chatMessage = ffilter (not . T.isPrefixOf "/") enteredE
      textCmd = fmapMaybe (T.stripPrefix "/") enteredE
  return (chatMessage, textCmd)
