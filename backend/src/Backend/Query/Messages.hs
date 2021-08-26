module Backend.Query.Messages where

import qualified Data.Map.Monoidal as Map
import Data.Map.Monoidal (MonoidalMap)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (Text)
import Data.Time
import Rhyolite.Backend.DB
import Rhyolite.Backend.DB.PsqlSimple
import Rhyolite.SemiMap

import Common.Schema
import Common.View

getMessages :: Db m => Set (Id Chatroom) -> m (MonoidalMap (Id Chatroom) (SemiMap UTCTime [MsgView]))
getMessages cs = do
  let chatrooms = In $ Set.toList cs
  results :: [(Id Chatroom, UTCTime, Text, Text)] <- [queryQ|
    select m.chatroom, m.timestamp, m.text, a.email
    from "Message" m
    join "Account" a on a.id = m.account
    where m.chatroom in ?chatrooms
  |]
  pure $ fmap SemiMap_Complete $ Map.unionsWith (<>) $
    flip fmap results $ \(cid, t, m, e) ->
      Map.singleton cid $ Map.singleton t [MsgView m e]
