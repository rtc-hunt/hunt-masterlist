{-# options_ghc -fno-warn-orphans #-}
module Backend.Listen where

import Control.Monad.Logger
import Data.Aeson
import Data.Aeson.GADT.TH
import Data.Constraint.Extras.TH
import Data.Functor.Identity
import qualified Data.Map.Monoidal as Map
import Data.Maybe
import Data.Pool
import Data.Proxy
import Data.Semigroup (First(..))
import Data.Text
import Data.These
import Data.These.Combinators
import Data.Time
import Data.Vessel
import Database.Beam
import Database.Beam.Postgres
import Database.PostgreSQL.Simple (Connection)
import Database.PostgreSQL.Simple.Beam ()
import Database.PostgreSQL.Simple.Class
import Obelisk.Route
-- import Rhyolite.Account
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
  Notify_Puzzle :: Notify (Change Puzzle)
  Notify_Solve :: Notify (Change Solution)
  Notify_Hunt :: Notify (Id Hunt)
  Notify_Tag :: Notify (Change Tag)
  Notify_Note :: Notify (Change Note)
  Notify_Meta :: Notify (Change Metapuzzle)
  Notify_ActiveUser :: Notify (Id ActiveUser)

deriveArgDict ''Notify
deriveJSONGADT ''Notify

instance HasNotification Notify Account where notification _ = Notify_Account

instance HasNotification Notify Chatroom where
  notification _ = Notify_Chatroom

instance HasNotification Notify Message where
  notification _ = Notify_Message

instance HasChangeNotification Notify Puzzle where
  changeNotification _ = Notify_Puzzle

instance HasChangeNotification Notify Solution where
  changeNotification _ = Notify_Solve

instance HasNotification Notify Hunt where
  notification _ = Notify_Hunt

instance HasChangeNotification Notify Tag where
  changeNotification _ = Notify_Tag

instance HasChangeNotification Notify Note where
  changeNotification _ = Notify_Note

instance HasChangeNotification Notify Metapuzzle where
  changeNotification _ = Notify_Meta

instance HasNotification Notify ActiveUser where
  notification _ = Notify_ActiveUser

getChatroom  :: Id Chatroom -> Pg (Maybe (Chatroom Identity))
getChatroom = runSelectReturningOne . lookup_ (_db_chatroom db)

notifyHandler
  :: Pool Connection
  -> DbNotification Notify
  -> PrivateChatV Proxy
  -> IO (PrivateChatV Identity)
notifyHandler pool nm v = case _dbNotification_message nm of
  Notify_Account :/ _ -> pure emptyV
  Notify_Hunt :/ _ -> pure emptyV
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
    V_Puzzle -> const $ pure emptyV
    V_HuntPuzzles -> const $ pure emptyV
    V_HuntMetas -> const $ pure emptyV
    V_Solutions -> const $ pure emptyV
    V_Tags -> const $ pure emptyV
    V_UniqueTags -> const $ pure emptyV
    V_Notes -> const $ pure emptyV
    V_Metas -> const $ pure emptyV
    V_ActiveUsers -> const $ pure emptyV
    V_Hunts -> const $ pure emptyV
  Notify_Message :/ mid -> do
    runNoLoggingT $ do
      msgs :: [(Id Chatroom, Int, UTCTime, Text, Text, Maybe Bool)] <- runDb pool $ [iquery|
        select m.message_chatroom__chatroom_id, m.mseq, m.message_timestamp, a.account_name, m.message_text, m."message_isMe"
        from (select *, row_number() over (partition by m.message_chatroom__chatroom_id order by message_timestamp) as mseq from db_message m) as m
        join db_account a on m.message_account__account_id = a.account_id
        where m.message_id = ${mid}
      |]
      case msgs of
        [] -> pure emptyV
        (cid, mseq, t, acc, txt, isme):_ -> buildV v $ \case
          V_Messages -> \sv ->
            let msg = Identity . SemiMap_Partial . Map.singleton mseq . First . Just $
                  Msg
                    { _msg_id = mid
                    , _msg_timestamp = t
                    , _msg_handle = acc
                    , _msg_text = txt
                    , _msg_isme = fromMaybe False isme }
            in case lookupSubVessel cid sv of
              Nothing -> pure emptyV
              Just (MapV reqs) -> pure . singletonSubVessel cid . MapV $
                flip Map.mapMaybeWithKey reqs $ \ri _ ->
                  if inRequestInterval ri mseq
                    then Just msg
                    else Nothing
          V_Chatroom -> const $ pure emptyV
          V_Chatrooms -> const $ pure emptyV
          V_Puzzle -> const $ pure emptyV
          V_HuntPuzzles -> const $ pure emptyV
          V_HuntMetas -> const $ pure emptyV
          V_Solutions -> const $ pure emptyV
          V_Tags -> const $ pure emptyV
          V_UniqueTags -> const $ pure emptyV
          V_Notes -> const $ pure emptyV
          V_Metas -> const $ pure emptyV
          V_ActiveUsers -> const $ pure emptyV
          V_Hunts -> const $ pure emptyV
  Notify_Puzzle :/ change@(Change pid theChange) -> buildV v $ \case
    V_Puzzle -> \(MapV cs) -> if Map.member pid cs
      then runDb pool (runSelectReturningOne $ lookup_ (_db_puzzles db) pid) >>= pure . \case
        Nothing -> emptyV
        Just c -> MapV $ Map.singleton pid $ pure (First c)
      else pure emptyV
    V_HuntPuzzles -> idSetByForeignKey _puzzle_Hunt _db_puzzles change
    V_HuntMetas -> \(MapV hs) -> case justThat theChange of
      Just puz | Map.member (_puzzle_Hunt puz) hs && _puzzle_IsMeta puz -> pure $ MapV $ Map.singleton (_puzzle_Hunt puz) $ pure $ SemiMap_Partial $ Map.singleton (primaryKey puz) $ First (Just $ _puzzle_Title puz)
      _ -> pure emptyV
-- idSetByForeignKey _puzzle_Hunt _db_puzzles change
    V_Chatroom -> const $ pure emptyV
    V_Chatrooms -> const $ pure emptyV
    V_Messages -> const $ pure emptyV
    V_Solutions -> const $ pure emptyV
    V_Tags -> const $ pure emptyV
    V_UniqueTags -> const $ pure emptyV
    V_Notes -> const $ pure emptyV
    V_Metas -> const $ pure emptyV
    V_ActiveUsers -> const $ pure emptyV
    V_Hunts -> const $ pure emptyV
  Notify_Solve :/ change -> buildV v $ \case
    V_Solutions -> byForeignKey _solution_Puzzle _db_solves change
    V_Chatroom -> const $ pure emptyV
    V_Chatrooms -> const $ pure emptyV
    V_Messages -> const $ pure emptyV
    V_Puzzle -> const $ pure emptyV
    V_HuntPuzzles -> const $ pure emptyV
    V_HuntMetas -> const $ pure emptyV
    V_Tags -> const $ pure emptyV
    V_UniqueTags -> const $ pure emptyV
    V_Notes -> const $ pure emptyV
    V_Metas -> const $ pure emptyV
    V_ActiveUsers -> const $ pure emptyV
    V_Hunts -> const $ pure emptyV
  Notify_Tag :/ change -> buildV v $ \case
    V_Tags -> byForeignKey _tag_Puzzle _db_tags change
    V_Chatroom -> const $ pure emptyV
    V_Chatrooms -> const $ pure emptyV
    V_Messages -> const $ pure emptyV
    V_Puzzle -> const $ pure emptyV
    V_HuntPuzzles -> const $ pure emptyV
    V_HuntMetas -> const $ pure emptyV
    V_Solutions -> const $ pure emptyV
    V_UniqueTags -> getUniqueTags change
    V_Notes -> const $ pure emptyV
    V_Metas -> const $ pure emptyV
    V_ActiveUsers -> const $ pure emptyV
    V_Hunts -> const $ pure emptyV
  Notify_Note :/ change -> buildV v $ \case
    V_Notes -> byForeignKey _note_Puzzle _db_notes change
    V_Chatroom -> const $ pure emptyV
    V_Chatrooms -> const $ pure emptyV
    V_Messages -> const $ pure emptyV
    V_Puzzle -> const $ pure emptyV
    V_HuntPuzzles -> const $ pure emptyV
    V_HuntMetas -> const $ pure emptyV
    V_Solutions -> const $ pure emptyV
    V_Tags -> const $ pure emptyV
    V_UniqueTags -> const $ pure emptyV
    V_Metas -> const $ pure emptyV
    V_ActiveUsers -> const $ pure emptyV
    V_Hunts -> const $ pure emptyV
  Notify_Meta :/ change -> buildV v $ \case
    V_Metas -> setByForeignKey _meta_Puzzle _db_metas change
    V_Chatroom -> const $ pure emptyV
    V_Chatrooms -> const $ pure emptyV
    V_Messages -> const $ pure emptyV
    V_Puzzle -> const $ pure emptyV
    V_HuntPuzzles -> const $ pure emptyV
    V_HuntMetas -> const $ pure emptyV
    V_Solutions -> const $ pure emptyV
    V_Tags -> const $ pure emptyV
    V_UniqueTags -> const $ pure emptyV
    V_Notes -> const $ pure emptyV
    V_ActiveUsers -> const $ pure emptyV
    V_Hunts -> const $ pure emptyV
  Notify_ActiveUser :/ auid -> buildV v $ \case
    V_Chatroom -> const $ pure emptyV
    V_Chatrooms -> const $ pure emptyV
    V_Messages -> const $ pure emptyV
    V_Puzzle -> const $ pure emptyV
    V_HuntPuzzles -> const $ pure emptyV
    V_HuntMetas -> const $ pure emptyV
    V_Solutions -> const $ pure emptyV
    V_Tags -> const $ pure emptyV
    V_UniqueTags -> const $ pure emptyV
    V_Notes -> const $ pure emptyV
    V_Metas -> const $ pure emptyV
    V_Hunts -> const $ pure emptyV
    V_ActiveUsers -> \(MapV cids) ->
        runDb pool $ do
          au <- runSelectReturningOne $ lookup_ (_db_activeUsers db) auid
          acc <- runSelectReturningOne $ lookup_ (_db_account db) (_activeUserId_user auid)
          pure $ case (au, acc) of
            (Just p, Just u) | Map.member (_activeUser_chat p) cids && _activeUser_openCount p > 0 -> MapV $ Map.singleton (_activeUserId_chat auid) $ pure $ SemiMap_Partial $ Map.singleton (_activeUserId_user auid) $ First (Just $ _account_name u)
            (Just p, Just u) | Map.member (_activeUser_chat p) cids -> MapV $ Map.singleton (_activeUserId_chat auid) $ pure $ SemiMap_Partial $ Map.singleton (_activeUserId_user auid) $ First Nothing
            _ -> emptyV
  where 
    nt = _dbNotification_notificationType nm
    byForeignKey 
      :: ( Ord fk
         , FromBackendRow Postgres (v Identity)
         , Database Postgres db
         , Table v
         , SqlValableTable Postgres (PrimaryKey v)
         , HasTableEquality Postgres (PrimaryKey v)
         )
      => (v Identity -> fk)
      -> (DatabaseSettings Postgres Db -> DatabaseEntity Postgres db (TableEntity v))
      -> Change v
      -> MapV fk v1 Proxy
      -> IO (MapV fk (SemiMap (PrimaryKey v Identity) (v Identity)) Identity)
    byForeignKey fk tbl (Change sid oldnew) = \(MapV ps) -> case nt of
      NotificationType_Delete -> case oldnew of
        This a | Map.member (fk a) ps -> pure $ MapV $ Map.singleton (fk a) $ pure $ SemiMap_Partial $ Map.singleton sid $ First (Nothing)
        _ -> pure emptyV
      _ -> do
        runDb pool (runSelectReturningOne $ lookup_ (tbl db) sid) >>= pure . \case
          Just p | Map.member (fk p) ps -> MapV $ Map.singleton (fk p) $ pure $ SemiMap_Partial $ Map.singleton sid $ First (Just p)
          _ -> emptyV
    setByForeignKey 
      :: ( Ord fk
         , FromBackendRow Postgres (v Identity)
         , Database Postgres db
         , Table v
         , SqlValableTable Postgres (PrimaryKey v)
         , HasTableEquality Postgres (PrimaryKey v)
         )
      => (v Identity -> fk)
      -> (DatabaseSettings Postgres Db -> DatabaseEntity Postgres db (TableEntity v))
      -> Change v
      -> MapV fk v1 Proxy
      -> IO (MapV fk (SemiSet (v Identity)) Identity)
    setByForeignKey fk tbl (Change sid oldnew) = \(MapV ps) -> case nt of
      NotificationType_Delete -> case oldnew of
        This a | Map.member (fk a) ps -> pure $ MapV $ Map.singleton (fk a) $ pure $ SemiMap_Partial $ Map.singleton a $ First (Nothing)
        _ -> pure emptyV
      _ -> do
        runDb pool (runSelectReturningOne $ lookup_ (tbl db) sid) >>= pure . \case
          Just p | Map.member (fk p) ps -> MapV $ Map.singleton (fk p) $ pure $ SemiMap_Partial $ Map.singleton p $ First (Just ())
          _ -> emptyV
    idSetByForeignKey 
      :: ( Ord fk
         , FromBackendRow Postgres (v Identity)
         , Database Postgres db
         , Table v
         , SqlValableTable Postgres (PrimaryKey v)
         , HasTableEquality Postgres (PrimaryKey v)
         )
      => (v Identity -> fk)
      -> (DatabaseSettings Postgres Db -> DatabaseEntity Postgres db (TableEntity v))
      -> Change v
      -> MapV fk v1 Proxy
      -> IO (MapV fk (SemiSet (Id v)) Identity)
    idSetByForeignKey fk tbl (Change sid oldnew) = \(MapV ps) -> case nt of
      NotificationType_Delete -> case oldnew of
        This a | Map.member (fk a) ps -> pure $ MapV $ Map.singleton (fk a) $ pure $ SemiMap_Partial $ Map.singleton (primaryKey a) $ First (Nothing)
        _ -> pure emptyV
      _ -> do
        runDb pool (runSelectReturningOne $ lookup_ (tbl db) sid) >>= pure . \case
          Just p | Map.member (fk p) ps -> MapV $ Map.singleton (fk p) $ pure $ SemiMap_Partial $ Map.singleton (primaryKey p) $ First (Just ())
          _ -> emptyV
    getUniqueTags 
      :: Change Tag 
      -> MapV () (SemiMap Text ()) Proxy
      -> IO (MapV () (SemiSet Text) Identity)
    getUniqueTags (Change sid _) = \(MapV _) -> do
        hasOtherInstance <- runDb pool (runSelectReturningOne $ select $ do
          tag <- all_ (_db_tags db)
          guard_ $ _tag_Tag tag ==. val_ (_tagId_Tag sid) &&. _tag_Puzzle tag /=. val_ (_tagId_Puzzle sid)
          return tag
          )
        pure $ case (hasOtherInstance, nt) of
            (Nothing, NotificationType_Delete) -> MapV $ Map.singleton () $ pure $ SemiMap_Partial $ Map.singleton (_tagId_Tag sid) $ First (Nothing)
            (Nothing, _) -> MapV $ Map.singleton () $ pure $ SemiMap_Partial $ Map.singleton (_tagId_Tag sid) $ First (Just ())
            (Just _, _) -> emptyV
