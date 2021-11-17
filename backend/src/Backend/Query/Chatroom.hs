module Backend.Query.Chatroom where

import Control.Monad
import qualified Data.Map.Monoidal as Map
import Data.Map.Monoidal (MonoidalMap)
import Data.Semigroup
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (Text)
import Rhyolite.Backend.DB
import Rhyolite.Backend.DB.PsqlSimple
import Rhyolite.SemiMap

import Common.Schema
import Common.View

searchForChatroom :: Db m => Set ChatroomQuery -> m (MonoidalMap ChatroomQuery (SemiMap (Id Chatroom) Text))
searchForChatroom qs = fmap (Map.unionsWith (<>)) $ forM (Set.toList qs) $ \q -> do
    let textQuery = "%" <> _chatroomQuery_search q <> "%"
    results <- [queryQ|
      select id, title
      from "Chatroom"
      where title ilike ?textQuery
      order by id desc
      limit 10
    |]
    pure $ Map.singleton q $ SemiMap_Complete $ Map.fromList results

getChatrooms :: Db m => Set (Id Chatroom) -> m (MonoidalMap (Id Chatroom) (First Text))
getChatrooms qs = do
    let cids = In $ Set.toList qs
    results :: [(Id Chatroom, Text)] <- [queryQ|
      select id, title
      from "Chatroom"
      where id in ?cids
    |]
    pure $ Map.fromList $ fmap (fmap First) results
