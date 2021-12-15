{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module Frontend.Templates.Partials.MessageView where

import Control.Monad
import Control.Monad.Fix
import Data.Map (Map)
import qualified Data.Map as Map
import qualified GHCJS.DOM.Element as El
import GHCJS.DOM.Types (MonadJSM, JSM, uncheckedCastTo, HTMLElement(..))
import Language.Javascript.JSaddle (eval, call, makeArgs, toJSVal)
import Reflex.Dom.Core

data MessageViewConfig t k a = MessageViewConfig
  { _messageViewConfig_minimumItemHeight :: Int
  -- ^ In pixels; used to estimate number of visible messages
  , _messageViewConfig_windowSize :: Dynamic t (Int, Int)
  -- ^ Width and height in pixels, used as an overestimate of the size
  -- of the visible client area of the message list since we don't
  -- have a binding to the ResizeObserver API.
  , _messageViewConfig_messages :: Dynamic t (Map k a)
  }

-- | Only used in the Client view of the message list
data MessageViewState t m = MessageViewState
  { _messageViewState_listEl :: Element EventResult (DomBuilderSpace m) t
  -- ^ The list Element that contains all of the messages whose scroll state
  -- needs to be managed.
  }

data MessageViewOutput t = MessageViewOutput
  { _messageViewOutput_viewportEstimate :: Dynamic t Int
  -- ^ In # of messages. Tries to be an overestimate of how many messages can fit in the visible client area.
  , _messageViewOutput_scrollNearTop :: Event t ()
  -- ^ Fires when the user has scrolled close to the top of the total content in the message list.
  , _messageViewOutput_jumpToBottom :: Event t ()
  -- ^ Fires when the user wants to jump to the bottom of the message view.
  }

emptyMessageViewOutput
  :: (Reflex t)
  => MessageViewOutput t
emptyMessageViewOutput = MessageViewOutput
  { _messageViewOutput_viewportEstimate = pure 24 -- Sure, why not.
  , _messageViewOutput_scrollNearTop = never
  , _messageViewOutput_jumpToBottom = never
  }

scrollRenderHook :: (MonadJSM m, El.IsElement (RawElement d)) => Element er d t -> m a -> m a
scrollRenderHook container domAction = do
  let e = _element_raw container
  result <- domAction
  h <- El.getScrollHeight e
  El.setScrollTop e h
  return result

messageView
  :: (DomBuilder t m, Prerender js t m, Ord k)
  => MessageViewConfig t k v
  -> (k -> Dynamic t v -> Client m ())
  -> m (MessageViewOutput t)
messageView cfg messageWidget = do
  let listAttrs = Map.fromList
        [ ("class", "p-4 flex-grow flex flex-col overflow-y-scroll")
        -- , ("style", "overflow-anchor: none") -- We want to manage the scroll behavior ourselves
        ]
  dOut <- prerender (pure emptyMessageViewOutput) $ do
    rec (listEl, _) <- elAttr' "ul" listAttrs . withRenderHook (scrollRenderHook listEl) $ do
          listWithKey (_messageViewConfig_messages cfg) $ messageWidget
    let st = MessageViewState
               { _messageViewState_listEl = listEl
               }
    windowHeight <- holdUniqDyn $ fmap snd (_messageViewConfig_windowSize cfg)
    jumpToBottom <- messageViewJumpToBottomHelper st {- TODO -} never
    messageViewResizeHelper cfg st
    scrollNearTop <- messageViewScrollHelper cfg st
    pure $ MessageViewOutput
      { _messageViewOutput_viewportEstimate = ffor windowHeight $ \wh ->
        (wh `quot` (_messageViewConfig_minimumItemHeight cfg) * 2) + 1
      , _messageViewOutput_scrollNearTop = scrollNearTop
      , _messageViewOutput_jumpToBottom = jumpToBottom
      }
  pure $ MessageViewOutput
    { _messageViewOutput_viewportEstimate = join $ fmap _messageViewOutput_viewportEstimate dOut
    , _messageViewOutput_scrollNearTop = switchDyn $ fmap _messageViewOutput_scrollNearTop dOut
    , _messageViewOutput_jumpToBottom = switchDyn $ fmap _messageViewOutput_jumpToBottom dOut
    }

messageViewScrollHelper
  :: (Reflex t, Monad m)
  => MessageViewConfig t k a
  -> MessageViewState t (Client m)
  -> m (Event t ())
messageViewScrollHelper cfg st = do
  let listEl = _messageViewState_listEl st
      windowHeightB = snd <$> current (_messageViewConfig_windowSize cfg)
      -- We fire an event if the scroll brings the scroll top close enough to the content height.
      scrollNearTop = fforMaybe (attach windowHeightB (domEvent Scroll listEl)) $ \(windowHeight, scrollTop) ->
        guard (scrollTop < fromIntegral windowHeight) >> pure ()
  pure scrollNearTop

messageViewJumpToBottomHelper
  :: (PerformEvent t m, MonadJSM (Performable m), DomBuilderSpace m ~ GhcjsDomSpace)
  => MessageViewState t m
  -> Event t ()
  -> m (Event t ())
messageViewJumpToBottomHelper state jumpE = do
  let messageList = _messageViewState_listEl state
      htmlEl = uncheckedCastTo HTMLElement $ _element_raw messageList
  performEvent $ ffor jumpE $ \_ -> do
    contentHeight <- El.getScrollHeight htmlEl
    El.setScrollTop htmlEl contentHeight

messageViewResizeHelper
  :: (Reflex t, MonadFix m, MonadJSM (Performable m), PerformEvent t m, MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace)
  => MessageViewConfig t k a
  -> MessageViewState t m
  -> m ()
messageViewResizeHelper cfg state = do
  let height = fst <$> _messageViewConfig_windowSize cfg
  -- When the height of the window changes that will have implications for
  -- the current scroll position of the message list. Here we extract an
  -- Event that fires with the delta in pixels of window height.
  -- It is never 0 but we do not rely on that.
  heightUpdate <- fmap fst . updated <$> scanDynMaybe
    ((,) 0)
    (\newHeight (_oldDelta, oldHeight) -> guard (newHeight /= oldHeight) >> pure (newHeight - oldHeight, newHeight))
    height
  let messageList = _messageViewState_listEl state
      htmlEl = uncheckedCastTo HTMLElement $ _element_raw messageList
  -- Whenever the window height changes we will adjust the scroll position of the message view.
  performEvent_ $ ffor heightUpdate $ \delta -> do
    -- The scroll height measures the total height of the content of the message list,
    -- even the parts that are not visible
    scrollHeight <- El.getScrollHeight htmlEl
    -- The client height measures the total height of the visible content of the message list.
    -- It includes padding, but not margin, borders, or the height of a horizontal scrollbar.
    -- We round because this can be a fractional value.
    clientHeight <- round <$> El.getClientHeight htmlEl
    -- The scroll top measures the distance in pixels from the top of the content to the top of the
    -- visible content. It is always greater than or equal to 0. 0 indicates that there is either no
    -- scroll bar or the element is scrolled to the very top.
    scrollTop <- El.getScrollTop htmlEl
    -- This is the amount of space in pixels between the bottom of the client area of the message list
    -- and the bottom of the total content. In particular, if it is 0, that means we are scrolled
    -- to the bottom of the message list.
    let leftoverHeight = scrollHeight - clientHeight - scrollTop
    -- TODO: Explain why this is the correct scrollTop adjustment
    El.setScrollTop htmlEl (scrollTop - min delta leftoverHeight)
    pure ()
  pure ()

messageViewRenderHook
  :: (DomBuilderSpace m ~ GhcjsDomSpace)
  => MessageViewState t m
  -> JSM x
  -> JSM x
messageViewRenderHook state action = do
  let messageList = _messageViewState_listEl state
      htmlEl = uncheckedCastTo HTMLElement $ _element_raw messageList
  -- TODO
  pre <- eval $ unlines
    [
    ]
  -- TODO
  post <- eval $ unlines
    [
    ]
  children <- call pre pre $ makeArgs $ toJSVal htmlEl
  x <- action
  _ <- call post post (htmlEl, children)
  pure x
