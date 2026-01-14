module Frontend.Chat where

import Reflex.Dom.Core
import Control.Monad.Fix
import Data.Functor.Identity
import Common.View
import Obelisk.Route.Frontend

import Rhyolite.Api (ApiRequest(..))
import Data.Vessel
import Control.Monad.IO.Class
import Control.Monad
import Data.Maybe
import qualified Data.Foldable as F
import qualified Data.Map as Map
import qualified Data.Text as T

import Common.Schema
import Common.Request
import Common.Route
import Frontend.Channel
import Frontend.Utils
import qualified Language.Javascript.JSaddle as JS
import qualified Templates.Partials.Message as Templates
import Frontend.Types
import Debug.Trace
import Control.Monad.Reader

chatOverlay :: (Monad m, MonadFix m, Reflex t, Adjustable t m, NotReady t m, PostBuild t m, DomBuilder t m, MonadHold t m
     , SetRoute t (R FrontendRoute) m, RouteToUrl (R FrontendRoute) m
     , PerformEvent t m
     , TriggerEvent t m
     , Prerender t m
     , SetRoute t (R FrontendRoute) (Client m)
     , AuthReq t m
     , AuthenticatedMonadQuery t m
     , MonadIO (Performable m)
     , MonadReader (Dynamic t (UserSettings Identity)) m
  ) => Bool -> Dynamic t (Maybe (Id Chatroom)) -> m ()
chatOverlay enableHotPopup channelId = do
    cv <- channelBuilder channelId
    let messages = join $ fromMaybe mempty <$> _channelView_messages cv
    popupsB <- onRender >>= delay 5 >>= hold enableHotPopup . (True <$)
    let newMessagesE = gate popupsB $ ffilter (not . Map.null) $ attachWith (flip Map.difference) (current messages) (updated messages)
    let recentMessages = (\m -> Map.drop (Map.size m - 2) m) <$> messages
    hideMessages <- fmap switchDyn $ prerender (pure never) $ debounce 3 newMessagesE
    finishHideMessages <- fmap switchDyn $ prerender (pure never) $ debounce 4 newMessagesE
    showMessagesD <- holdDyn 0 $ leftmost [ 2 <$ newMessagesE, 1 <$ hideMessages, 0 <$ finishHideMessages ]
    let chatOverlayClass = ffor showMessagesD $ \i -> "class" =: ("chat-overlay scrollable flex flex-col hide-state-" <> (T.pack $ show i)) 
    elDynAttr "div" chatOverlayClass $ do
      void $ listWithKey (recentMessages) $ \_ -> Templates.message 
    
    enableNotif <- asks $ fmap _userSettings_enableNotifications
    prerender_ blank $ performEvent_ $ ffor (gate (current enableNotif) $ attachPromptlyDyn (_channelView_name cv) (coincidence $ updated recentMessages <$ newMessagesE)) $ \(title, messages) -> void $ JS.liftJSM $ do
            JS.jsg2 ("flingMessages" :: T.Text) (JS.toJSVal $ fromMaybe "NOTITLE" title) $ JS.toJSVal $ F.fold $ (\a -> _msg_handle a <> ": " <> _msg_text a <> "\n") <$> messages

