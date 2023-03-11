{-# Language DeriveAnyClass #-}
{-# Language DeriveGeneric #-}
{-# Language StandaloneDeriving #-}
{-# Language QuantifiedConstraints #-}
{-# Language TypeApplications #-}
{-# options_ghc -fno-warn-orphans #-}
{-# options_ghc -fno-warn-incomplete-uni-patterns #-}
module Backend.Schema where

import Data.Functor.Identity
import Data.Int
import Data.Time
import Database.Beam
import Database.Beam.AutoMigrate hiding (zipTables, Table)
import Database.Beam.Backend.SQL.Types
import Database.Beam.Postgres
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Crypto.JOSE.JWK

import Common.Schema

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
  , _db_googleKeys :: f (TableEntity GoogleKey)
  }
  deriving (Generic, Database be)

db :: DatabaseSettings Postgres Db
db = defaultDbSettings

dbAnn :: AnnotatedDatabaseSettings Postgres Db
dbAnn = defaultAnnotatedDbSettings db

runMigrations :: Connection -> IO ()
runMigrations conn = tryRunMigrationsWithEditUpdate dbAnn conn

instance FromField (SqlSerial Int64) where
  fromField f mbs = fmap SqlSerial $ fromField f mbs

instance FromField (PrimaryKey Chatroom Identity) where
  fromField f mbs = ChatroomId <$> fromField f mbs

instance FromField (PrimaryKey Message Identity) where
  fromField f mbs = MessageId <$> fromField f mbs

instance ToField a => ToField (SqlSerial a) where
  toField (SqlSerial a) = toField a

instance ToField (PrimaryKey Chatroom Identity) where
  toField (ChatroomId x) = toField x

instance ToField (PrimaryKey Message Identity) where
  toField (MessageId x) = toField x

data GoogleKey f = GoogleKey
  { _googleKey_fetchedAt :: Columnar f UTCTime
  , _googleKey_expires :: Columnar f UTCTime
  , _googleKey_keyset :: Columnar f (PgJSON JWKSet)
  } deriving (Generic, Beamable)

instance Table GoogleKey where
  data PrimaryKey GoogleKey f = GoogleKeyId { _googleKeyId_fetchedAt :: Columnar f UTCTime }
    deriving (Generic, Beamable)
  primaryKey au = GoogleKeyId (_googleKey_fetchedAt au)

deriving instance Show (PrimaryKey GoogleKey Identity)
deriving instance Eq (PrimaryKey GoogleKey Identity)
deriving instance Ord (PrimaryKey GoogleKey Identity)
