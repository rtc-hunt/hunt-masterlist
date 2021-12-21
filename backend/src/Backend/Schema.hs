{-# options_ghc -fno-warn-orphans #-}
{-# options_ghc -fno-warn-incomplete-uni-patterns #-}
module Backend.Schema where

import Common.Schema
import Database.Groundhog.TH
import Database.Id.Groundhog.TH
import Rhyolite.Account.Groundhog ()
import Rhyolite.DB.Groundhog.TH

mkRhyolitePersist (Just "migrateSchema") [groundhog|
  - entity: Chatroom
  - entity: Message
|]

makeDefaultKeyIdInt64 ''Chatroom 'ChatroomKey
makeDefaultKeyIdInt64 ''Message 'MessageKey
