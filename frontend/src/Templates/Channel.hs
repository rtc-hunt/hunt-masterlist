module Templates.Channel where

import Control.Category
import Data.Bool
import Data.Set (Set)
import qualified Data.Sequence as Seq
import Data.Sequence (ViewR (..))
import Data.Semigroup
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Vessel
import Database.Id.Class
import Obelisk.Route.Frontend
import Prelude hiding ((.), id)
import Reflex
import Reflex.Dom.Core hiding (Request, El)
import Rhyolite.Api hiding (Request)
import Rhyolite.Vessel.Path
import Control.Monad.IO.Class

import Control.Monad
import Control.Monad.Fix
import Data.Map (Map)
import qualified Data.Map as Map
import qualified GHCJS.DOM.Element as JS
import GHCJS.DOM.Types (MonadJSM)

import Common.Request
import Common.Route
import Common.Schema
import Common.View

import Templates.Types
import Templates.Partials.Buttons
import Templates.Partials.ChannelList
import Frontend.Utils

import qualified Data.Set as Set

channelPage ::
  ( PostBuild t m
  , DomBuilder t m
  , MonadQuery t (Vessel V (Const SelectedCount)) m
  , MonadHold t m
  , MonadFix m
  , SetRoute t (R FrontendRoute) m
  , Requester t m, Response m ~ Identity, Request m ~ ApiRequest () PublicRequest PrivateRequest
  )
  => ChannelConfig t m
  -> m (ChannelOut t m)
channelPage cfg = elClass "div" "w-full flex flex-row flex-grow h-0" $ do
  rec cl <- channelList (ChannelListConfig (channelSearchResults cl))
  cout <- channel cfg
  pure cout


data ChannelConfig t m = ChannelConfig
  { _channelConfig_name :: Dynamic t Text
  , _channelConfig_clearInput :: Event t ()
  , _channelConfig_messagesConfig :: MessagesConfig m
  }

data ChannelOut t m = ChannelOut
  { _channelOut_messageInput :: InputEl t m
  , _channelOut_send :: Event t ()
  , _channelOut_scrollingContainer :: El t m
  }

channel :: Template t m => ChannelConfig t m -> m (ChannelOut t m)
channel cfg = elClass "div" "w-full flex flex-col" $ do
  elClass "div" "p-4 bg-raised flex flex-row justify-between items-center border-b border-metaline relative" $ do
    elClass "div" "flex flex-col" $ do
      elClass "div" "font-karla font-bold text-h2 md:text-h1 text-copy leading-none" $
        dynText $ _channelConfig_name cfg
  mout <- messages $ _channelConfig_messagesConfig cfg
  elClass "div" "p-1 bg-white flex flex-row justify-center border-t border-metaline" $ do
    input <- messageInput $ _channelConfig_clearInput cfg
    send <- sendButton
    pure $ ChannelOut
      { _channelOut_messageInput = input
      , _channelOut_send = send
      , _channelOut_scrollingContainer = _messagesOut_container mout
      }

messageInput :: DomBuilder t m => Event t () -> m (InputEl t m)
messageInput clearEvent = inputElement $ def
  & initialAttributes .~
    ( "class" =: "focus:outline-none mx-1 font-facit font-label text-label placeholder-light px-3.5 bg-inset rounded shadow-input flex-grow"
      <> "placeholder" =: "Type your message"
      <> "type" =: "text"
    )
  & inputElementConfig_setValue .~ ("" <$ clearEvent)

sendButton :: DomBuilder t m => m (Event t ())
sendButton = iconButton "send"

data MessagesConfig m = MessagesConfig
  { _messagesConfig_messageList :: m ()
  }

data MessagesOut t m = MessagesOut
  { _messagesOut_container :: El t m
  }

messages :: Template t m => MessagesConfig m -> m (MessagesOut t m)
messages cfg = do
  (e, _) <- elAttr' "ul" ("class" =: "p-4 flex-grow flex flex-col overflow-y-scroll") $ do
    _messagesConfig_messageList cfg
  pure $ MessagesOut
    { _messagesOut_container = e
    }

messagesHelper
  :: (DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m, DomRenderHook t m, JS.IsElement (RawElement (DomBuilderSpace m)))
  => El t m
  -> Dynamic t (Map Int MsgView)
  -> m ()
messagesHelper scrollEl msgs =
  withRenderHook (scrollRenderHook scrollEl) $ void $ listWithKey msgs $ \_ -> message

scrollRenderHook :: (MonadJSM m, JS.IsElement (RawElement d)) => Element er d t -> m a -> m a
scrollRenderHook container domAction = do
  let e = _element_raw container
  result <- domAction
  h <- JS.getScrollHeight e
  JS.setScrollTop e h
  return result

message
  :: (PostBuild t m, DomBuilder t m)
  => Dynamic t MsgView
  -> m ()
message mView = do
  elClass "li" "font-facit text-copy flex flex-col mb-6 ml-16 md:ml-0 md:items-end" $ do
    elClass "div" "flex flex-row items-baseline justify-between" $ do
      elClass "div" "text-label md:mr-4" $ dynText (fmap _msgView_handle mView)
      elClass "div" "font-bold text-label text-light" $ dynText . ffor mView $ \mv -> T.pack $
        formatTime defaultTimeLocale "%R" (_msgView_timestamp mv)
    elClass "div" "p-4 rounded border border-metaline bg-white w-auto" $ dynText (fmap _msgView_text mView)

data ChannelView t = ChannelView
  { _channelView_name :: Dynamic t (Maybe Text)
  , _channelView_messages :: Dynamic t (Maybe (Dynamic t (Map Int MsgView)))
  }

channelBuilder
  :: forall t m js.
     ( PostBuild t m
     , DomBuilder t m
     , MonadHold t m
     , MonadFix m
     , Prerender js t m
     , MonadQuery t (Vessel V (Const SelectedCount)) m
     , PerformEvent t m
     , TriggerEvent t m
     , MonadIO (Performable m)
     , Requester t m, Response m ~ Identity, Request m ~ ApiRequest () PublicRequest PrivateRequest
     )
  => Dynamic t (Id Chatroom)
  -> m (ChannelView t)
channelBuilder cid = do
  pb <- onRender
  let cidE = leftmost
        [ tag (current cid) pb
        , updated cid
        ]
  (_errors, channelInfo) <- fmap fanEither $ requestingIdentity $ ffor cidE $ \c ->
    ApiRequest_Private () $ PrivateRequest_LatestMessage c
  (name, msgs) <- fmap splitDynPure $ widgetHold (pure mempty) $ ffor channelInfo $ \(c, n) -> do
    name <- watch $ pure $ key V_Chatroom ~> key c ~> firstP
    rec let requestCount = 100
            nearToEnd :: Int -> Int -> Bool
            nearToEnd lastMessage maxAllowed = lastMessage + 20 >= maxAllowed
            interval0 = Set.singleton (RequestInterval n requestCount requestCount)
            loadBottom :: Event t (Set RequestInterval -> Set RequestInterval)
            loadBottom = fforMaybe (mMessagesE) $ \msgs -> do
                ms :: Map RequestInterval (Map Int MsgView) <- msgs
                (ri, ms') <- Map.lookupMax ms
                (k, _) <- Map.lookupMax ms'
                let m = requestIntervalMax ri
                guard (nearToEnd k m)
                return $ Set.insert (RequestInterval m 0 requestCount)
        intervals <- foldDyn ($) interval0 loadBottom
        mMessagesE <- fmapMaybe ((\case (_ :> x) -> Just x; _ -> Nothing) . Seq.viewr) <$> batchOccurrences 1 (updated mMessages')
        mMessages' :: Dynamic t (Maybe (Map RequestInterval (Map Int MsgView))) <- watch . ffor intervals $ \ris ->
          key V_Messages ~> key c ~> keys ris ~> semiMapsP
    channelMessages <- maybeDyn $ fmap (fmap Map.unions) mMessages'
    pure (name, channelMessages)
  pure $ ChannelView
    { _channelView_name = join name
    , _channelView_messages = join msgs
    }
