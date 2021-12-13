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
import Database.Groundhog
import Database.Id.Groundhog
import Database.PostgreSQL.Simple (Connection)
import Obelisk.Route
import Rhyolite.Backend.DB
import Rhyolite.Backend.DB.PsqlSimple
import Rhyolite.Backend.Listen
import Rhyolite.SemiMap

import Backend.Query
import Backend.Schema ()
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

notifyHandler
  :: Pool Connection
  -> DbNotification Notify
  -> PrivateChatV Proxy
  -> IO (PrivateChatV Identity)
notifyHandler db nm v = case _dbNotification_message nm of
  Notify_Account :/ _ -> pure emptyV
  Notify_Chatroom :/ cid -> buildV v $ \case
    V_Chatrooms -> \(MapV queries) -> do
      results :: Map.MonoidalMap ChatroomQuery (SemiMap (Id Chatroom) Text) <- runNoLoggingT $ runDb (Identity db) $ searchForChatroom $ Map.keysSet queries
      let x = if Map.null results
            then emptyV
            else MapV $ pure <$> results
      pure x
    V_Chatroom -> \(MapV cs) -> if Map.member cid cs
      then runNoLoggingT $ runDb (Identity db) (get $ fromId cid) >>= pure . \case
        Nothing -> emptyV
        Just c -> MapV $ Map.singleton cid $ pure (First (_chatroom_title c))
      else pure emptyV
    V_Messages -> const $ pure emptyV
  Notify_Message :/ mid -> do
    runNoLoggingT $ do
      let messageId = mid
      msgs :: [(Id Chatroom, Int, UTCTime, Text, Text)] <- runDb (Identity db) $ [queryQ|
        select m.chatroom, m.mseq, m.timestamp at time zone 'utc', a.account_email, m.text
        from (select *, row_number() over (partition by m.chatroom) as mseq from "Message" m) as m
        join "Account" a on m.account = a.id
        where m.id = ?messageId
      |]
      case msgs of
        [] -> pure emptyV
        (cid, mseq, time, acc, txt):_ -> buildV v $ \case
          V_Messages -> \sv ->
            let msg = Identity . SemiMap_Partial . Map.singleton (time, mid) . First . Just $
                  MsgView
                    { _msgView_sequence = mseq
                    , _msgView_handle = acc
                    , _msgView_text = txt }
            in case lookupSubVessel cid sv of
              Nothing -> pure emptyV
              Just (MapV reqs) -> pure . singletonSubVessel cid . MapV $
                flip Map.mapMaybeWithKey reqs $ \ri _ ->
                  if inRequestInterval ri mseq
                    then Just msg
                    else Nothing
          V_Chatroom -> const $ pure emptyV
          V_Chatrooms -> const $ pure emptyV
