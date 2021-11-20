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

getMessages :: Db m => Set (Id Chatroom) -> m (MonoidalMap (Id Chatroom) (SemiMap (UTCTime, Id Message) MsgView))
getMessages cs = do
  let chatrooms = In $ Set.toList cs
  results :: [(Id Chatroom, UTCTime, Id Message, Text, Text)] <- [queryQ|
    select m.chatroom, m.timestamp at time zone 'utc', m.id, m.text, a.account_email
    from "Message" m
    join "Account" a on a.id = m.account
    where m.chatroom in ?chatrooms
  |]
  let responseTemplate = Map.fromSet (const (SemiMap_Complete Map.empty)) cs
      responseValues = fmap SemiMap_Complete $
        -- The `unionWith const` is safe because `mid` is a primary key
        Map.unionsWith (Map.unionWith const) $
          flip fmap results $ \(cid, t, mid, messageBody, senderHandle) ->
            Map.singleton cid $ Map.singleton (t, mid) $ MsgView
              { _msgView_handle = senderHandle
              , _msgView_text = messageBody
              }
  pure $
    -- The sql query will not return results for empty channels. We ensure that
    -- queries for the contents of those channels also receive a response
    -- letting them know that the channel is empty (i.e., by sending
    -- 'SemiMap_Complete Map.empty')
    Map.unionWith const responseValues responseTemplate
