{-# Language DeriveAnyClass #-}
{-# Language StandaloneDeriving #-}
module Common.Schema where

import Data.Aeson
import Data.Functor.Identity
import Data.Int (Int64)
import Data.Signed
import Data.Text
import Data.Time
import Database.Beam.Backend.SQL.Types
import Database.Beam.Schema
import GHC.Generics
import Rhyolite.Account

data Db f = Db
  { _db_account :: f (TableEntity Account)
  , _db_chatroom :: f (TableEntity Chatroom)
  , _db_message :: f (TableEntity Message)
  }
  deriving (Generic, Database be)

type Id a = PrimaryKey a Identity

type AuthToken = Signed (PrimaryKey Account Identity)

data Chatroom f = Chatroom
  { _chatroom_id :: Columnar f (SqlSerial Int64)
  , _chatroom_title :: Columnar f Text
  }
  deriving (Generic, Beamable)

instance Table Chatroom where
  newtype PrimaryKey Chatroom f = ChatroomId { unChatroomId :: Columnar f (SqlSerial Int64) }
    deriving (Generic, Beamable)
  primaryKey = ChatroomId . _chatroom_id

deriving instance Eq (PrimaryKey Chatroom Identity)
deriving instance Ord (PrimaryKey Chatroom Identity)
deriving instance Show (PrimaryKey Chatroom Identity)
instance ToJSON (PrimaryKey Chatroom Identity)
instance FromJSON (PrimaryKey Chatroom Identity)
instance ToJSONKey (PrimaryKey Chatroom Identity)
instance FromJSONKey (PrimaryKey Chatroom Identity)


data Message f = Message
  { _message_id :: Columnar f (SqlSerial Int64)
  , _message_chatroom :: PrimaryKey Chatroom f
  , _message_text :: Columnar f Text
  , _message_timestamp :: Columnar f UTCTime
  , _message_account :: PrimaryKey Account f
  }
  deriving (Generic, Beamable)

instance Table Message where
  newtype PrimaryKey Message f = MessageId { unMessageId :: Columnar f (SqlSerial Int64) }
    deriving (Generic, Beamable)
  primaryKey = MessageId . _message_id

deriving instance Show (PrimaryKey Message Identity)
deriving instance Eq (PrimaryKey Message Identity)
instance ToJSON (PrimaryKey Message Identity)
instance FromJSON (PrimaryKey Message Identity)
instance ToJSONKey (PrimaryKey Message Identity)
instance FromJSONKey (PrimaryKey Message Identity)
