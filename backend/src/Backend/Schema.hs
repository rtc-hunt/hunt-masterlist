{-# Language TypeApplications #-}
{-# options_ghc -fno-warn-orphans #-}
{-# options_ghc -fno-warn-incomplete-uni-patterns #-}
module Backend.Schema where

import Data.Functor.Identity
import Data.Int
import Database.Beam
import Database.Beam.AutoMigrate hiding (zipTables)
import Database.Beam.Backend.SQL.Types
import Database.Beam.Postgres
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField

import Common.Schema

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
