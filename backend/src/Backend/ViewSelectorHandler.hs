{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Backend.ViewSelectorHandler where

import Common.App
import qualified Data.Map as Map
import qualified Data.Map.Monoidal as MMap
import Database.Id.Groundhog
import Database.Groundhog
import Rhyolite.Backend.App
import Rhyolite.SemiMap
import Control.Lens
import Data.Semigroup (First (..))
import Rhyolite.Backend.Logging
import Rhyolite.Backend.DB
import Backend.Schema
import Data.Pool
import Database.PostgreSQL.Simple
import Database.Id.Class
import Common.Schema
import Control.Monad.Logger

viewSelectorHandler :: LoggingEnv -> Pool Connection -> QueryHandler (HMLViewSelector a) IO
viewSelectorHandler logger db = QueryHandler $ \vs -> runLoggingEnv logger $ runDb (Identity db) $ do
  let puzzleVS = _hmlViewSelector_puzzle vs
  puzzleV <- iforM puzzleVS $ \pq q -> fmap (q,) $ queryPuzzles pq
  
  let solveVS = _hmlViewSelector_solve vs
  solveV <- iforM solveVS $ \pq q -> fmap (q,) $ querySolves pq
  
  return HMLView
    { _hmlView_echo = MMap.mapWithKey (\_ v -> (v, First (Just ""))) $ _hmlViewSelector_echo vs
    , _hmlView_puzzle = puzzleV
    , _hmlView_solve = solveV
    }

queryPuzzles :: (MonadLogger m, PersistBackend m) => PuzzleQuery -> m (SemiMap (Id Puzzle) Puzzle)
queryPuzzles = \case
  PuzzleQuery_AllPuzzles -> do
    $(logDebugS) "" ""
    allPuzzles <- fmap (\(a,b) -> (toId a, b)) <$> project (AutoKeyField, PuzzleConstructor) CondEmpty 
    let puzzlesMap = MMap.MonoidalMap $ Map.fromList allPuzzles
    return $ SemiMap_Complete puzzlesMap
  PuzzleQuery_byId puzzleId -> do
    $(logDebugS) "" ""
    thePuzzles <- fmap (\(a,b) -> (toId a, b)) <$> project (AutoKeyField, PuzzleConstructor) (AutoKeyField ==. fromId puzzleId)
    let puzzlesMap = MMap.MonoidalMap $ Map.fromList thePuzzles
    return $ SemiMap_Complete puzzlesMap

querySolves :: (MonadLogger m, PersistBackend m) => SolveQuery -> m (SemiMap (Id Solve) Solve)
querySolves = \case
  SolveQuery_byPuzzle puzzle -> do
    $(logDebugS) "" ""
    allSolves <- fmap (\(a,b) -> (toId a, b)) <$> project (AutoKeyField, SolveConstructor) (Solve_PuzzleField ==. puzzle)
    let puzzlesMap = MMap.MonoidalMap $ Map.fromList allSolves
    return $ SemiMap_Complete puzzlesMap

