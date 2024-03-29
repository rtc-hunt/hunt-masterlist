{-# OPTIONS_GHC -Wno-unused-matches -Wno-unused-local-binds #-}

module Backend.View.Chatroom where

import Control.Monad
import qualified Data.Map.Monoidal as Map
import Data.Map.Monoidal (MonoidalMap)
import Data.Semigroup
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (Text)
import Database.PostgreSQL.Simple.Types
import Database.PostgreSQL.Simple.Class
import Rhyolite.SemiMap

import Backend.Schema ()
import Common.Schema
import Common.View

searchForChatroom :: Psql m => Set ChatroomQuery -> m (MonoidalMap ChatroomQuery (SemiMap (Id Chatroom) Text))
searchForChatroom qs = fmap (Map.unionsWith (<>)) $ forM (Set.toList qs) $ \q -> do
    let textQuery = "%" <> _chatroomQuery_search q <> "%"
    results <- [iquery|
      select chatroom_id, chatroom_title
      from db_chatroom
      where chatroom_title ilike ${textQuery}
      order by chatroom_id desc
      limit 10
    |]
    pure $ Map.singleton q $ SemiMap_Complete $ Map.fromList results

getChatrooms :: Psql m => Set (Id Chatroom) -> m (MonoidalMap (Id Chatroom) (First Text))
getChatrooms qs = do
    let cids = In $ Set.toList qs
    results :: [(Id Chatroom, Text)] <- [iquery|
      select chatroom_id, chatroom_title
      from db_chatroom
      where chatroom_id in ${cids}
    |]
    pure $ Map.fromList $ fmap (fmap First) results
