{-# options_ghc -fno-warn-orphans #-}
module Backend.Listen where

import Control.Monad.Logger
import Data.Aeson
import Data.Aeson.GADT.TH
import Data.Constraint.Extras.TH
import Data.Functor.Identity
import qualified Data.Map.Monoidal as Map
import Data.Pool
import Data.Proxy
import Data.Semigroup (First(..))
import Data.Text
import Data.Time
import Data.Vessel
import Database.Beam
import Database.Beam.Postgres
import Database.PostgreSQL.Simple (Connection)
import Database.PostgreSQL.Simple.Beam ()
import Database.PostgreSQL.Simple.Class
import Obelisk.Route
import Rhyolite.Account
import Rhyolite.DB.NotifyListen
import Rhyolite.DB.NotifyListen.Beam
import Rhyolite.SemiMap

import Backend.Db
import Backend.Schema
import Backend.View.Chatroom (searchForChatroom)
import Common.Schema
import Common.View

data Notify a where
  Notify_Account :: Notify (Id Account)
  Notify_Chatroom :: Notify (Id Chatroom)
  Notify_Message :: Notify (Id Message)

deriveArgDict ''Notify
deriveJSONGADT ''Notify

instance HasNotification Notify Account where notification _ = Notify_Account

instance HasNotification Notify Chatroom where
  notification _ = Notify_Chatroom

instance HasNotification Notify Message where
  notification _ = Notify_Message

getChatroom  :: Id Chatroom -> Pg (Maybe (Chatroom Identity))
getChatroom cid = do
  runSelectReturningOne $ select $ filter_ (\x -> primaryKey x ==. val_ cid) $ all_ (_db_chatroom db)

notifyHandler
  :: Pool Connection
  -> DbNotification Notify
  -> PrivateChatV Proxy
  -> IO (PrivateChatV Identity)
notifyHandler pool nm v = case _dbNotification_message nm of
  Notify_Account :/ _ -> pure emptyV
  Notify_Chatroom :/ cid -> buildV v $ \case
    V_Chatrooms -> \(MapV queries) -> do
      results :: Map.MonoidalMap ChatroomQuery (SemiMap (Id Chatroom) Text) <- runDb pool $ searchForChatroom $ Map.keysSet queries
      let x = if Map.null results
            then emptyV
            else MapV $ pure <$> results
      pure x
    V_Chatroom -> \(MapV cs) -> if Map.member cid cs
      then runDb pool (getChatroom cid) >>= pure . \case
        Nothing -> emptyV
        Just c -> MapV $ Map.singleton cid $ pure (First (_chatroom_title c))
      else pure emptyV
    V_Messages -> const $ pure emptyV
  Notify_Message :/ mid -> do
    runNoLoggingT $ do
      msgs :: [(Id Chatroom, Int, UTCTime, Text, Text)] <- runDb pool $ [iquery|
        select m.message_chatroom__chatroom_id, m.mseq, m.message_timestamp, a.account_email, m.message_text
        from (select *, row_number() over (partition by m.message_chatroom__chatroom_id order by message_timestamp) as mseq from db_message m) as m
        join db_account a on m.message_account__account_id = a.account_id
        where m.message_id = ${mid}
      |]
      case msgs of
        [] -> pure emptyV
        (cid, mseq, t, acc, txt):_ -> buildV v $ \case
          V_Messages -> \sv ->
            let msg = Identity . SemiMap_Partial . Map.singleton mseq . First . Just $
                  Msg
                    { _msg_id = mid
                    , _msg_timestamp = t
                    , _msg_handle = acc
                    , _msg_text = txt }
            in case lookupSubVessel cid sv of
              Nothing -> pure emptyV
              Just (MapV reqs) -> pure . singletonSubVessel cid . MapV $
                flip Map.mapMaybeWithKey reqs $ \ri _ ->
                  if inRequestInterval ri mseq
                    then Just msg
                    else Nothing
          V_Chatroom -> const $ pure emptyV
          V_Chatrooms -> const $ pure emptyV
