module Frontend.UserSettings where

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
import Rhyolite.Api (ApiRequest(..))
import Rhyolite.Frontend.App
import Common.Request
import Frontend.Types

userSettingsPanel :: (Monad m, DomBuilder t m, MonadFix m, PostBuild t m, MonadHold t m, Prerender t m, MonadReader (Dynamic t (UserSettings Identity)) m
     , AuthReq t m
   )
   => m (Event t ())
userSettingsPanel = mdo
   -- dialogEl <- fmap fst $ elAttr' "dialog" ("id" =: "myModal" <> "closedby" =: "any") $ mdo
     elClass "h1" "ui header centered" $ text "Settings"
     usD <- ask
     -- dyn $ ffor usD $ \(UserSettings { _userSettings_chatMuted, _userSettings_chatSidebar, _userSettings_enableNotifications, _userSettings_enableAudio, _userSettings_openInNewTab }) ->
     newNick <- el "div" $ do
       newNick <- inputElement $ def
       clicked <- button "set new nick"
       pure $ current (value newNick) <@ clicked
     requesting_ $ ApiRequest_Private () . PrivateRequest_Renick <$> newNick

     enableNotifications <- semToggle "Enable Notifications" $ _userSettings_enableNotifications <$> usD
     -- openInNewTab <- semToggle "Open links in new tab" $ _userSettings_openInNewTab <$> usD
     elClass "h3" "ui header" $ text "Defaults - override in corner menu"
     chatMuted <- semToggle "Mute chats" $ _userSettings_chatMuted <$> usD
     chatSidebar <- semToggle "Open sidebar chat" $ _userSettings_chatSidebar <$> usD
           
     cancel <- button "Cancel"
     saveSettings <- button "Save and Close"

     let newSettings = current $ UserSettings <$> (_userSettings_currentHunt <$> usD) <*> chatMuted <*> chatSidebar <*> enableNotifications <*> (_userSettings_enableAudio <$> usD) <*> (_userSettings_openInNewTab <$> usD)

     let saveEvt = (ApiRequest_Private () . PrivateRequest_SaveSettings) <$> newSettings <@ saveSettings
     saveResult <- requestingIdentity saveEvt
     lastSave <- holdDyn (Right ()) saveResult
     
     pure $ () <$ leftmost [saveSettings, cancel]
