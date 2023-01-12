{-# Language TypeApplications #-}
{-# Language StandaloneDeriving #-}
{-# Language GeneralizedNewtypeDeriving #-}
module Backend.View where

import Control.Lens.Indexed
import Data.Constraint.Extras
import Data.Functor.Identity
import qualified Data.Map.Monoidal as Map
import Data.Int
import Data.Pool
import Data.Semigroup
import Data.Maybe
import Data.Vessel
import Data.Vessel.SubVessel
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Beam ()
import Database.Beam
import Database.Beam.Backend.SQL.Types
import Database.Beam.Postgres
import Database.PostgreSQL.Simple.Beam ()
import Data.Signed.ClientSession
import Rhyolite.DB.NotifyListen.Beam
import Rhyolite.SemiMap
import Rhyolite.Vessel.AuthMapV
import Rhyolite.Vessel.ErrorV
import Rhyolite.Vessel.ErrorV.Internal (ErrorV(..))
import qualified Data.Signed
import qualified Data.Map.Monoidal
import qualified Reflex.Query.Class
import qualified Common.View
import Rhyolite.Backend.App
import Web.ClientSession as CS
import qualified Data.Foldable as DF

import Backend.Db
import Backend.Schema
import Backend.View.Chatroom
import Backend.View.Messages
import Backend.Listen

import Common.Schema
import Common.View

privateQueryHandler
  :: Pool Connection
  -> PrivateChatV Proxy
  -> IO (PrivateChatV Identity)
privateQueryHandler pool q = (>>= (\a -> print "Passed" >> return a)) $ (print q >>) $ buildV q $ \case
  V_Chatrooms -> \(MapV qs) -> do
    chatrooms <- runDb pool $ searchForChatroom $ Map.keysSet qs
    pure $ MapV $ pure <$> chatrooms
  V_Chatroom -> \(MapV cs) -> do
    MapV . fmap pure <$> runDb pool (getChatrooms $ Map.keysSet cs)
  V_Messages -> \sv -> do
    rs <- runDb pool $
      getMessages . fmap (Map.keysSet . unMapV) . getSubVessel $ sv
    pure $ mkSubVessel . fmap (MapV . fmap Identity) $ rs
  V_Puzzle -> \(MapV ps) ->
    fmap (MapV . fmap (pure . First) . Map.fromList) $ runDb pool $ runSelectReturningList $ select $ do
      puzzle <- all_ (_db_puzzles db)
      guard_ $ _puzzle_id puzzle `in_` (val_ . _puzzleId_id <$> Map.keys ps)
      return (primaryKey puzzle, puzzle)
  V_HuntPuzzles -> \(MapV hs) ->
    fmap MapV $ ifor hs $ \h _ -> runDb pool $ fmap (Identity . SemiMap_Complete . Map.fromList . fmap (\a -> (a, ()))) $ runSelectReturningList $ select $ do
      puzzle <- all_ (_db_puzzles db)
      guard_ $ _puzzle_Hunt puzzle ==. val_ h
      guard_ $ _puzzle_removed puzzle /=. val_ (Just True)
      return $ primaryKey puzzle
  V_Solutions -> \(MapV hs) ->
    fmap MapV $ ifor hs $ \h _ -> runDb pool $ fmap (Identity . SemiMap_Complete . Map.fromList . fmap (\a -> (primaryKey a, a))) $ runSelectReturningList $ select $ do
      puzzle <- all_ (_db_solves db)
      guard_ $ _solution_Puzzle puzzle ==. val_ h
      return puzzle
  V_Tags -> \(MapV hs) ->
    fmap MapV $ ifor hs $ \h _ -> runDb pool $ fmap (Identity . SemiMap_Complete . Map.fromList . fmap (\a -> (primaryKey a, a))) $ runSelectReturningList $ select $ do
      puzzle <- all_ (_db_tags db)
      guard_ $ _tag_Puzzle puzzle ==. val_ h
      return puzzle
  V_Notes -> \(MapV hs) ->
    fmap MapV $ ifor hs $ \h _ -> runDb pool $ fmap (Identity . SemiMap_Complete . Map.fromList . fmap (\a -> (primaryKey a, a))) $ runSelectReturningList $ select $ do
      puzzle <- all_ (_db_notes db)
      guard_ $ _note_Puzzle puzzle ==. val_ h
      return puzzle
  V_UniqueTags -> \(MapV hs) ->
    fmap MapV $ ifor hs $ \_ _ -> runDb pool $ fmap (Identity . SemiMap_Complete . Map.fromList . fmap (\a -> (a, ()))) $ runSelectReturningList $ select $ nub_ $ do
      tag <- all_ (_db_tags db)
      return $ _tag_Tag tag
  V_Metas -> \(MapV hs) ->
    fmap MapV $ ifor hs $ \h _ -> runDb pool $ fmap (Identity . SemiMap_Complete . Map.fromList . fmap (\a -> (a, ()))) $ runSelectReturningList $ select $ do
      meta <- all_ (_db_metas db)
      guard_ $ _meta_Puzzle meta ==. val_ h
      return $ meta
  V_HuntMetas -> \(MapV hs) ->
    fmap MapV $ ifor hs $ \h _ -> runDb pool $ fmap (Identity . SemiMap_Complete . Map.fromList) $ runSelectReturningList $ select $ do
      puzzle <- all_ (_db_puzzles db)
      guard_ $ _puzzle_Hunt puzzle ==. val_ h
      guard_ $ _puzzle_IsMeta puzzle ==. val_ True
      guard_ $ _puzzle_removed puzzle /=. val_ (Just True)
      return $ (primaryKey puzzle, _puzzle_Title puzzle)
  V_ActiveUsers -> \(MapV hs) ->
    fmap MapV $ ifor hs $ \h _ -> runDb pool $ fmap (Identity . SemiMap_Complete . Map.fromList) $ runSelectReturningList $ select $ do
      u <- all_ (_db_account db)
      au <- oneToOne_ (_db_activeUsers db) _activeUser_user u
      guard_ $ _activeUser_chat au ==. val_ h
      guard_ $ _activeUser_openCount au >. val_ 0
      return $ (primaryKey u, _account_name u)
  V_Hunts -> \(MapV hs) ->
    fmap MapV $ ifor hs $ \_ _ -> runDb pool $ fmap (Identity . SemiMap_Complete . Map.fromList) $ runSelectReturningList $ select $ nub_ $ do
      hunt <- all_ (_db_hunt db)
      return $ (primaryKey hunt, hunt)
  V_LiveHunts -> \(MapV hs) ->
    fmap MapV $ ifor hs $ \_ _ -> runDb pool $ fmap (Identity . SemiMap_Complete . Map.fromList . fmap (\a -> (a, ()))) $ runSelectReturningList $ select $ nub_ $ do
      hunt <- all_ (_db_hunt db)
      guard_ $ _hunt_live hunt ==. val_ True
      return $ primaryKey hunt

trackActiveUsers
  :: CS.Key
  -> Pool Connection
  -> Data.Map.Monoidal.MonoidalMap ClientKey (AuthMapV (Data.Signed.Signed (Id Account)) PrivateChatV (Const Reflex.Query.Class.SelectedCount))
  -> IO ()
trackActiveUsers csk pool query = do
    let v = DF.fold query
    let acct = fmap getChatCount $ getSubVessel $ unAuthMapV v
    iforM acct $ \someToken someMapM -> do
      let acctIdM = readSignedWithKey csk someToken
      iforM someMapM $ \_ someMap -> iforM someMap $ \someChat someCount -> do
        iforM acctIdM $ \_ acctId -> updateActiveUsers acctId someChat someCount
    return ()
  where
    getChatCount someV = (lookupV ErrorVK_View $ unErrorV someV) >>= lookupV V_Chatroom >>= pure . fmap getConst . unMapV
    updateActiveUsers :: Id Account -> Id Chatroom -> Reflex.Query.Class.SelectedCount -> IO ()
    updateActiveUsers acctId someChat someCount = do
        runDb pool $ do
          currentCount <- runSelectReturningOne $
            lookup_ (_db_activeUsers db) $ ActiveUserId someChat acctId
          case currentCount of
            Just current -> updateAndNotify @_ @_ @_ @Notify (_db_activeUsers db) (primaryKey current) $ \a -> _activeUser_openCount a <-. (current_ (_activeUser_openCount a) + val_ (fromIntegral someCount))
            Nothing -> do
                 insertAndNotify @_ @_ @_ @Notify (_db_activeUsers db) $ 
                   ActiveUser (val_ someChat) (val_ acctId) (val_ $ fromIntegral someCount)
          return ()
        print $ ("Updating Active User Count For: ", acctId, someChat, someCount)



