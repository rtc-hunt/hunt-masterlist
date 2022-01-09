{-# Language TypeApplications #-}
module Backend.View where

import Control.Lens.Indexed
import Data.Functor.Identity
import qualified Data.Map.Monoidal as Map
import Data.Int
import Data.Pool
import Data.Semigroup
import Data.Vessel
import Data.Vessel.SubVessel
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Beam ()
import Database.Beam
import Database.Beam.Backend.SQL.Types
import Database.Beam.Postgres
import Database.PostgreSQL.Simple.Beam ()
import Rhyolite.DB.NotifyListen.Beam
import Rhyolite.SemiMap

import Backend.Db
import Backend.Schema
import Backend.View.Chatroom
import Backend.View.Messages

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

