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

getMessages :: Db m
            => MonoidalMap (Id Chatroom) (Set RequestInterval)
            -> m (MonoidalMap (Id Chatroom) (MonoidalMap RequestInterval (SemiMap (UTCTime, Id Message) MsgView)))
getMessages reqs = do
  let requestValues = Values ["int4", "int4", "int4", "int4"] $ do
        (cid, ris) <- Map.toList reqs
        RequestInterval mid before after <- Set.toList ris
        pure (cid, mid, before, after)
  results :: [(Id Chatroom, Int, Int, Int, Int, UTCTime, Id Message, Text, Text)] <- [queryQ|
    select r.cid, r.seq, r.before, r.after,
           m.seq, m.timestamp at time zone 'utc', m.id, m.text, a.account_email
    from (select m.*, row_number() over (partition by m.chatroom) as seq from "Message" m) as m
    join "Account" a on a.id = m.account
    join ?requestValues as r (cid,seq,before,after)
      on r.cid = m.chatroom
      and (r.seq - r.before <= m.seq)
      and (m.seq <= r.seq + r.after)
  |]
  let responseTemplate = Map.map (Map.fromSet (const (SemiMap_Complete Map.empty))) reqs
      responseValues = fmap (fmap SemiMap_Complete) $
        -- The `unionWith const` is safe because `mid` is a primary key
        Map.unionsWith (Map.unionWith (Map.unionWith const)) $
          flip fmap results $ \(cid, rseq, before, after, mseq, t, mid, messageBody, senderHandle) ->
            Map.singleton cid $ Map.singleton (RequestInterval rseq before after) $ Map.singleton (t, mid) $ MsgView
              { _msgView_sequence = mseq
              , _msgView_handle = senderHandle
              , _msgView_text = messageBody
              }
  pure $
    -- The sql query will not return results for empty channels. We ensure that
    -- queries for the contents of those channels also receive a response
    -- letting them know that the channel is empty (i.e., by sending
    -- 'SemiMap_Complete Map.empty')
    Map.unionWith const responseValues responseTemplate
