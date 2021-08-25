module Common.Schema
  ( module X
  , module Common.Schema
  ) where

import Database.Id.Class as X
import Data.Text
import Data.Time
import Rhyolite.Account as X

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
