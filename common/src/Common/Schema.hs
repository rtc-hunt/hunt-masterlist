module Common.Schema where

import Database.Id.Class
import Data.Text
import Data.Time
import Rhyolite.Account

data Chatroom = Chatroom
  { _chatroom_title :: Text
  }

instance HasId Chatroom

data Message = Message
  { _message_chatroom :: Id Chatroom
  , _message_text :: Text
  , _message_timestamp :: UTCTime
  , _message_account :: Id Account
  }

instance HasId Message
