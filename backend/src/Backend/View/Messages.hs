{-# OPTIONS_GHC -Wno-unused-matches -Wno-unused-local-binds #-}

module Backend.View.Messages where

import qualified Data.Map.Monoidal as Map
import Data.Map.Monoidal (MonoidalMap)
import Data.Maybe
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (Text)
import Data.Time
import Rhyolite.SemiMap
import Database.PostgreSQL.Simple.Types
import Database.PostgreSQL.Simple.Class

import Backend.Schema ()
import Common.Schema
import Common.View

getMessages :: Psql m
            => MonoidalMap (Id Chatroom) (Set RequestInterval)
            -> m (MonoidalMap (Id Chatroom) (MonoidalMap RequestInterval (SemiMap Int Msg)))
getMessages reqs = do
  let requestValues = Values ["int4", "int4", "int4", "int4"] $ do
        (cid, ris) <- Map.toList reqs
        RequestInterval mid before after <- Set.toList ris
        pure (cid, mid, before, after)
  results :: [(Id Chatroom, Int, Int, Int, Int, UTCTime, Id Message, Text, Maybe Bool, Text)] <- [iquery|
    select r.cid, r.seq, r.before, r.after,
           m.seq, m.message_timestamp, m.message_id, m.message_text, m."message_isMe", a.account_name
    from (select m.*, row_number() over (partition by m.message_chatroom__chatroom_id order by message_timestamp) as seq from db_message m) as m
    join db_account a on a.account_id = m.message_account__account_id
    join ${requestValues} as r (cid,seq,before,after)
      on r.cid = m.message_chatroom__chatroom_id
      and (r.seq - r.before <= m.seq)
      and (m.seq <= r.seq + r.after)
  |]
  let responseTemplate = Map.map (Map.fromSet (const (SemiMap_Complete Map.empty))) reqs
      responseValues = fmap (fmap SemiMap_Complete) $
        -- The `unionWith const` is safe because `mid` is a primary key
        Map.unionsWith (Map.unionWith (Map.unionWith const)) $
          flip fmap results $ \(cid, rseq, before, after, mseq, t, mid, messageBody, isme, senderHandle) ->
            Map.singleton cid $ Map.singleton (RequestInterval rseq before after) $ Map.singleton mseq $ Msg
              { _msg_id = mid
              , _msg_timestamp = t
              , _msg_handle = senderHandle
              , _msg_text = messageBody
              , _msg_isme = fromMaybe False isme
              }
  pure $
    -- The sql query will not return results for empty channels. We ensure that
    -- queries for the contents of those channels also receive a response
    -- letting them know that the channel is empty (i.e., by sending
    -- 'SemiMap_Complete Map.empty')
    Map.unionWith const responseValues responseTemplate
