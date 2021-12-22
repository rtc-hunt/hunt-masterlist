module Templates.Channel where

import Data.Text (Text)
import Reflex.Dom.Core hiding (El, Request)

import Frontend.Types
import Templates.Partials.Buttons
import Templates.Partials.ChannelList
import Templates.Partials.Containers
import Templates.Partials.Headers
import Templates.Partials.Inputs
import Templates.Types

data ChannelConfig t m = ChannelConfig
  { _channelConfig_name :: Dynamic t Text
  , _channelConfig_clearInput :: Event t ()
  , _channelConfig_messagesConfig :: MessagesConfig m
  , _channelConfig_channelList :: ChannelListConfig m
  }

data ChannelOut t m = ChannelOut
  { _channelOut_messageInput :: InputEl t m
  , _channelOut_send :: Event t ()
  , _channelOut_scrollingContainer :: El t m
  , _channelOut_logout :: Event t Logout
  , _channelOut_channelList :: ChannelList t m
  }

channel :: Template t m => ChannelConfig t m -> m (ChannelOut t m)
channel cfg = screenContainer $ do
  logout <- header
  elClass "div" "w-full flex flex-row flex-grow h-0" $ do
    chanList <- channelList (_channelConfig_channelList cfg)
    elClass "div" "w-full flex flex-col" $ do
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
          , _channelOut_logout = logout
          , _channelOut_channelList = chanList
          }

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
