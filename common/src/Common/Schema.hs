{-# Language DeriveAnyClass #-}
{-# Language StandaloneDeriving #-}
{-# Language QuantifiedConstraints #-}

module Common.Schema where

import Data.Aeson
import Data.Default
import Data.Functor.Identity
import Data.Int (Int64)
import Data.Signed
import Data.Text
import Data.Time
import Database.Beam.Backend.SQL.Types
import Database.Beam.Schema
import GHC.Generics
-- import Rhyolite.Account

data Db f = Db
  { _db_account :: f (TableEntity Account)
  , _db_chatroom :: f (TableEntity Chatroom)
  , _db_message :: f (TableEntity Message)
  , _db_puzzles :: f (TableEntity Puzzle)
  , _db_solves :: f (TableEntity Solution)
  , _db_hunt :: f (TableEntity Hunt)
  , _db_tags :: f (TableEntity Tag)
  , _db_notes :: f (TableEntity Note)
  , _db_metas :: f (TableEntity Metapuzzle)
  , _db_activeUsers :: f (TableEntity ActiveUser)
  }
  deriving (Generic, Database be)

type Id a = PrimaryKey a Identity

type AuthToken = Signed (PrimaryKey Account Identity)

data Account f = Account
  { _account_id :: Columnar f (SqlSerial Int64)
  , _account_name :: Columnar f Text
  , _account_guid :: Columnar f Text
  }
  deriving (Generic, Beamable)

instance Table Account where
  newtype PrimaryKey Account f = AccountId { unAccountId :: Columnar f (SqlSerial Int64) }
    deriving (Generic, Beamable)
  primaryKey = AccountId . _account_id

deriving instance Eq (PrimaryKey Account Identity)
deriving instance Ord (PrimaryKey Account Identity)
deriving instance Show (PrimaryKey Account Identity)
instance ToJSON (PrimaryKey Account Identity)
instance FromJSON (PrimaryKey Account Identity)
instance ToJSONKey (PrimaryKey Account Identity)
instance FromJSONKey (PrimaryKey Account Identity)
instance ToJSON (PrimaryKey Account (Nullable Identity))
instance FromJSON (PrimaryKey Account (Nullable Identity))
deriving instance Eq (PrimaryKey Account (Nullable Identity))

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
deriving instance Show (PrimaryKey Chatroom (Nullable Identity))
instance ToJSON (PrimaryKey Chatroom Identity)
instance FromJSON (PrimaryKey Chatroom Identity)
instance ToJSONKey (PrimaryKey Chatroom Identity)
instance FromJSONKey (PrimaryKey Chatroom Identity)
instance ToJSON (PrimaryKey Chatroom (Nullable Identity))
instance FromJSON (PrimaryKey Chatroom (Nullable Identity))
deriving instance Eq (PrimaryKey Chatroom (Nullable Identity))


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

data Puzzle f = Puzzle
  { _puzzle_id :: Columnar f (SqlSerial Int64)
  , _puzzle_Title :: Columnar f Text
  , _puzzle_URI :: Columnar f Text
  , _puzzle_SheetURI :: Columnar f (Maybe Text)
  , _puzzle_IsMeta :: Columnar f Bool
  , _puzzle_StartedAt :: Columnar f (Maybe UTCTime)
  , _puzzle_Channel :: PrimaryKey Chatroom (Nullable f)
  , _puzzle_Hunt :: PrimaryKey Hunt f
  }
  deriving (Generic, Beamable)

instance Table Puzzle where
  newtype PrimaryKey Puzzle f = PuzzleId { _puzzleId_id :: Columnar f (SqlSerial Int64) }
    deriving (Generic, Beamable)
  primaryKey = PuzzleId . _puzzle_id

deriving instance Show (PrimaryKey Puzzle Identity)
deriving instance Eq (PrimaryKey Puzzle Identity)
deriving instance Ord (PrimaryKey Puzzle Identity)
instance ToJSON (PrimaryKey Puzzle Identity)
instance FromJSON (PrimaryKey Puzzle Identity)
instance ToJSONKey (PrimaryKey Puzzle Identity)
instance FromJSONKey (PrimaryKey Puzzle Identity)
deriving instance Show (Puzzle Identity)
deriving instance Eq (Puzzle Identity)
instance ToJSON (Puzzle Identity)
instance FromJSON (Puzzle Identity)

data Solution f = Solution
  { _solution_Puzzle :: (PrimaryKey Puzzle f)
  , _solution_Solution :: Columnar f Text
  , _solution_IsBacksolve :: Columnar f Bool
  }
  deriving (Generic, Beamable)

instance Table Solution where
  data PrimaryKey Solution f = SolutionId { _solveId_Puzzle :: (PrimaryKey Puzzle f), _solveId_Solution :: Columnar f Text }
    deriving (Generic, Beamable)
  primaryKey slv = SolutionId (_solution_Puzzle slv) (_solution_Solution slv)

deriving instance Show (PrimaryKey Solution Identity)
deriving instance Eq (PrimaryKey Solution Identity)
deriving instance Ord (PrimaryKey Solution Identity)
instance ToJSON (PrimaryKey Solution Identity)
instance FromJSON (PrimaryKey Solution Identity)
instance ToJSONKey (PrimaryKey Solution Identity)
instance FromJSONKey (PrimaryKey Solution Identity)
deriving instance Eq (Solution Identity)
instance ToJSON (Solution Identity)
instance FromJSON (Solution Identity)

data Metapuzzle f = Metapuzzle
  { _meta_Puzzle :: (PrimaryKey Puzzle f)
  , _meta_Metapuzzle :: (PrimaryKey Puzzle f)
  }
  deriving (Generic, Beamable)

instance Table Metapuzzle where
  data PrimaryKey Metapuzzle f = MetapuzzleId { _metaId_Puzzle :: (PrimaryKey Puzzle f), _metaId_Metapuzzle :: (PrimaryKey Puzzle f) }
    deriving (Generic, Beamable)
  primaryKey hbt = MetapuzzleId (_meta_Puzzle hbt) (_meta_Metapuzzle hbt)

deriving instance Show (PrimaryKey Metapuzzle Identity)
deriving instance Eq (PrimaryKey Metapuzzle Identity)
deriving instance Ord (PrimaryKey Metapuzzle Identity)
instance ToJSON (PrimaryKey Metapuzzle Identity)
instance FromJSON (PrimaryKey Metapuzzle Identity)
instance ToJSONKey (PrimaryKey Metapuzzle Identity)
instance FromJSONKey (PrimaryKey Metapuzzle Identity)
deriving instance Show (Metapuzzle Identity)
deriving instance Eq (Metapuzzle Identity)
deriving instance Ord (Metapuzzle Identity)
instance ToJSON (Metapuzzle Identity)
instance FromJSON (Metapuzzle Identity)
instance ToJSONKey (Metapuzzle Identity)
instance FromJSONKey (Metapuzzle Identity)

data Tag f = Tag
  { _tag_Puzzle :: (PrimaryKey Puzzle f)
  , _tag_Tag :: Columnar f Text
  }
  deriving (Generic, Beamable)

instance Table Tag where
  data PrimaryKey Tag f = TagId { _tagId_Puzzle :: (PrimaryKey Puzzle f), _tagId_Tag :: Columnar f Text }
    deriving (Generic, Beamable)
  primaryKey slv = TagId (_tag_Puzzle slv) (_tag_Tag slv)

deriving instance Show (PrimaryKey Tag Identity)
deriving instance Eq (PrimaryKey Tag Identity)
deriving instance Ord (PrimaryKey Tag Identity)
instance ToJSON (PrimaryKey Tag Identity)
instance FromJSON (PrimaryKey Tag Identity)
instance ToJSONKey (PrimaryKey Tag Identity)
instance FromJSONKey (PrimaryKey Tag Identity)
deriving instance Eq (Tag Identity)
instance ToJSON (Tag Identity)
instance FromJSON (Tag Identity)

data Note f = Note
  { _note_id :: Columnar f (SqlSerial Int64)
  , _note_Puzzle :: (PrimaryKey Puzzle f)
  , _note_Note :: Columnar f Text
  }
  deriving (Generic, Beamable)

instance Table Note where
  newtype PrimaryKey Note f = NoteId { _noteId_id :: Columnar f (SqlSerial Int64) }
    deriving (Generic, Beamable)
  primaryKey = NoteId . _note_id

deriving instance Show (PrimaryKey Note Identity)
deriving instance Eq (PrimaryKey Note Identity)
deriving instance Ord (PrimaryKey Note Identity)
instance ToJSON (PrimaryKey Note Identity)
instance FromJSON (PrimaryKey Note Identity)
instance ToJSONKey (PrimaryKey Note Identity)
instance FromJSONKey (PrimaryKey Note Identity)
deriving instance Eq (Note Identity)
instance ToJSON (Note Identity)
instance FromJSON (Note Identity)

data Hunt f = Hunt
  { _hunt_id :: Columnar f (SqlSerial Int64)
  , _hunt_title :: Columnar f Text
  , _hunt_rootpage :: Columnar f Text
  , _hunt_channel :: PrimaryKey Chatroom f
  }
  deriving (Generic, Beamable)

instance Table Hunt where
  newtype PrimaryKey Hunt f = HuntId { _huntId_id :: Columnar f (SqlSerial Int64) }
    deriving (Generic, Beamable)
  primaryKey = HuntId . _hunt_id

deriving instance Show (PrimaryKey Hunt Identity)
deriving instance Eq (PrimaryKey Hunt Identity)
deriving instance Ord (PrimaryKey Hunt Identity)
instance ToJSON (PrimaryKey Hunt Identity)
instance FromJSON (PrimaryKey Hunt Identity)
instance ToJSONKey (PrimaryKey Hunt Identity)
instance FromJSONKey (PrimaryKey Hunt Identity)
instance ToJSON (Hunt Identity)
instance FromJSON (Hunt Identity)

data ActiveUser f = ActiveUser
  { _activeUser_chat :: PrimaryKey Chatroom f
  , _activeUser_user :: PrimaryKey Account f
  , _activeUser_openCount :: Columnar f Int64
  } deriving (Generic, Beamable)

instance Table ActiveUser where
  data PrimaryKey ActiveUser f = ActiveUserId { _activeUserId_chat :: PrimaryKey Chatroom f, _activeUserId_user :: PrimaryKey Account f }
    deriving (Generic, Beamable)
  primaryKey au = ActiveUserId (_activeUser_chat au) (_activeUser_user au)

deriving instance Show (PrimaryKey ActiveUser Identity)
deriving instance Eq (PrimaryKey ActiveUser Identity)
deriving instance Ord (PrimaryKey ActiveUser Identity)
instance ToJSON (PrimaryKey ActiveUser Identity)
instance FromJSON (PrimaryKey ActiveUser Identity)
instance ToJSONKey (PrimaryKey ActiveUser Identity)
instance FromJSONKey (PrimaryKey ActiveUser Identity)
instance ToJSON (ActiveUser Identity)
instance FromJSON (ActiveUser Identity)
