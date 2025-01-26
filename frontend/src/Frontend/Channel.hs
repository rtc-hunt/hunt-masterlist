module Frontend.Channel where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Data.Functor.Identity
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Sequence (ViewR(..))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Text (Text)
import Data.Vessel
import qualified GHCJS.DOM.Element as JS
import GHCJS.DOM.Types (MonadJSM)
import Obelisk.Route
import Obelisk.Route.Frontend
import Reflex.Dom.Core hiding (El)
import Rhyolite.Api (ApiRequest(..))
import Rhyolite.Frontend.App
import Rhyolite.Vessel.AuthenticatedV
import Rhyolite.Vessel.Path

import Templates (ChannelConfig(..), ChannelOut(..), MessagesConfig(..))
import qualified Templates as Templates
import Templates.Partials.ChannelList
import Templates.Partials.Message as Templates
import Templates.Types

import Common.Request
import Common.Route
import Common.Schema
import Common.View
import Rhyolite.Frontend.Auth.App

import Frontend.ChannelList

import Frontend.Types
import Frontend.Utils

channel ::
  ( Monad m
  , MonadIO (Performable m)
  , MonadFix m
  , MonadHold t m
  , PostBuild t m
  , AuthReq t m
  , AuthReq t (Client m)
  , Routed t (Maybe (Id Chatroom)) m
  , DomBuilder t m
  , Prerender t m
  , SetRoute t (R FrontendRoute) (Client m)
  , AuthenticatedMonadQuery t m
  , TriggerEvent t m
  , PerformEvent t m
  )
  => m (Event t Logout) -- AuthAppWidget MasterlistApp t m (Event t Logout)
channel = do
  mcid <- askRoute
  channelView <- channelBuilder mcid
  logout <- prerender (pure never) $ do
    rec
      ChannelOut input send msgs logout clist <- Templates.channel $
        ChannelConfig
          { _channelConfig_name = fromMaybe "" <$> _channelView_name channelView
          , _channelConfig_clearInput = clickOrEnter
          , _channelConfig_messagesConfig = MessagesConfig $ do
              dyn_ $ ffor (_channelView_messages channelView) $ \case
                Nothing -> dynText $ ffor mcid $ \case
                  Nothing -> "⬅️ Select or create a channeel"
                  Just _ -> ""
                Just ms -> messagesHelper (Templates._messagesOut_container msgs) ms
          , _channelConfig_channelList = ChannelListConfig $
              channelSearchResults clist
          }
      let newMessage = current $ value input
          enter = keypress Enter $ _inputElement_element input
          clickOrEnter = leftmost [send, enter]
          sendMessage = gate (not . T.null . T.strip <$> newMessage) clickOrEnter
          mkMsgReq c m = case c of
            Nothing -> Nothing
            Just c' -> Just $ ApiRequest_Private () $ PrivateRequest_SendMessage c' m
      _ <- requestingIdentity $ attachWithMaybe
        (\(c, m) _ -> mkMsgReq c m)
        ((,) <$> current mcid <*> newMessage)
        sendMessage
    pure logout
  pure $ switchDyn logout

embeddableChannel ::
  ( Monad m
  , MonadIO (Performable m)
  , MonadFix m
  , MonadHold t m
  , PostBuild t m
  , PerformEvent t m
  , TriggerEvent t m
  , DomBuilder t m
  , Prerender t m
  , SetRoute t (R FrontendRoute) (Client m)
  , AuthenticatedMonadQuery t m
  , AuthReq t m
  , AuthReq t (Client m)
  , TriggerEvent t m
  , PerformEvent t m
  )
  => Dynamic t (Maybe (Id Chatroom))-> m ()
embeddableChannel mcid = mdo      
    channelView <- channelBuilder mcid
    _ <- prerender (pure ()) $ do
      rec
        ChannelOut input send msgs logout clist <- Templates.channel $
          ChannelConfig
            { _channelConfig_name = fromMaybe "" <$> _channelView_name channelView
            , _channelConfig_clearInput = clickOrEnter
            , _channelConfig_messagesConfig = MessagesConfig $ do
                dyn_ $ ffor (_channelView_messages channelView) $ \case
                  Nothing -> dynText $ ffor mcid $ \case
                    Nothing -> "⬅️ Select or create a channeel"
                    Just _ -> ""
                  Just ms -> messagesHelper (Templates._messagesOut_container msgs) ms
            , _channelConfig_channelList = ChannelListConfig $
                channelSearchResults clist
            }
        let newMessage = current $ value input
            enter = keypress Enter $ _inputElement_element input
            clickOrEnter = leftmost [send, enter]
            sendMessage = gate (not . T.null . T.strip <$> newMessage) clickOrEnter
            mkMsgReq c m = case c of
              Nothing -> Nothing
              Just c' -> Just $ ApiRequest_Private () $ PrivateRequest_SendMessage c' m
        _ <- requestingIdentity $ attachWithMaybe
          (\(c, m) _ -> mkMsgReq c m)
          ((,) <$> current mcid <*> newMessage)
          sendMessage
        blank
      blank
    blank

data ChannelView t = ChannelView
  { _channelView_name :: Dynamic t (Maybe Text)
  , _channelView_messages :: Dynamic t (Maybe (Dynamic t (Map Int Msg)))
  }

channelBuilder
  :: forall t m js.
     ( PostBuild t m
     , DomBuilder t m
     , MonadHold t m
     , MonadFix m
     , Prerender t m
     , AuthenticatedMonadQuery t m
     , AuthReq t m
     , MonadIO (Performable m)
     , TriggerEvent t m
     , PerformEvent t m
     )
  => Dynamic t (Maybe (Id Chatroom))
  -> m (ChannelView t) -- AuthAppWidget MasterlistApp t m (ChannelView t)
channelBuilder cid = do
  pb <- onRender
  let cidE = fmapMaybe id $ leftmost
        [ tag (current cid) pb
        , updated cid
        ]
  (_errors, channelInfo) <- fmap fanEither $ requestingIdentity $ ffor cidE $ \c ->
    ApiRequest_Private () $ PrivateRequest_LatestMessage c
  (name, msgs) <- fmap splitDynPure $ widgetHold (pure mempty) $ ffor channelInfo $ \(c, n) -> do
    name <- watch $ pure $ privateP ~> key V_Chatroom ~> key c ~> firstP
    rec let requestCount = 100
            nearToEnd :: Int -> Int -> Bool
            nearToEnd lastMessage maxAllowed = lastMessage + 20 >= maxAllowed
            interval0 = Set.singleton (RequestInterval n requestCount requestCount)
            loadBottom :: Event t (Set RequestInterval -> Set RequestInterval)
            loadBottom = fforMaybe (mMessagesE) $ \msgs -> do
                ms :: Map RequestInterval (Map Int Msg) <- msgs
                (ri, ms') <- Map.lookupMax ms
                (k, _) <- Map.lookupMax ms'
                let m = requestIntervalMax ri
                guard (nearToEnd k m)
                return $ Set.insert (RequestInterval m 0 requestCount)
        intervals <- foldDyn ($) interval0 loadBottom
        mMessagesE <- fmapMaybe ((\case (_ :> x) -> Just x; _ -> Nothing) . Seq.viewr) <$> batchOccurrences 1 (updated mMessages')
        mMessages' :: Dynamic t (Maybe (Map RequestInterval (Map Int Msg))) <- watch . ffor intervals $ \ris ->
          privateP ~> key V_Messages ~> key c ~> keys ris ~> semiMapsP
    channelMessages <- maybeDyn $ fmap (fmap Map.unions) mMessages'
    pure (name, channelMessages)
  pure $ ChannelView
    { _channelView_name = join name
    , _channelView_messages = join msgs
    }

messagesHelper
  :: (DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m, DomRenderHook t m, JS.IsElement (RawElement (DomBuilderSpace m)))
  => El t m
  -> Dynamic t (Map Int Msg)
  -> m ()
messagesHelper scrollEl msgs =
  withRenderHook (scrollRenderHook scrollEl) $ void $ listWithKey msgs $ \_ -> Templates.message

scrollRenderHook :: (MonadJSM m, JS.IsElement (RawElement d)) => Element er d t -> m a -> m a
scrollRenderHook container domAction = do
  let e = _element_raw container
  result <- domAction
  h <- JS.getScrollHeight e
  JS.setScrollTop e h
  return result
