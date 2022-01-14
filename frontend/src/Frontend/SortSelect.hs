{-# OPTIONS_GHC -Werror=incomplete-patterns #-}
{-# Language LambdaCase #-}

module Frontend.SortSelect where

import Control.Monad
import Reflex
import qualified Data.Array as A
import Data.Map as Map
import Data.Graph
import Data.Tree
import Data.Foldable as F

import Frontend.Types
import Common.Schema

import Debug.Trace

toSortKeys
  :: forall t. (Reflex t)
  => Dynamic t PuzzleOrdering
  -> Dynamic t (Map (Id Puzzle) (PuzzleData t))
  -> Dynamic t (Map (PuzzleSortKey) (PuzzleData t))
toSortKeys puzzleOrdering puzzleData = join $ toSortKeysInner <$> puzzleOrdering <*> puzzleData
  where
    toSortKeysInner :: PuzzleOrdering -> Map (Id Puzzle) (PuzzleData t) -> Dynamic t (Map PuzzleSortKey (PuzzleData t))
    toSortKeysInner = \case
      PuzzleOrdering_Any -> pure . Map.mapKeys PuzzleSortKey_Id
      PuzzleOrdering_ByMeta -> \pdMap -> do
        rootElems <- fmap (Map.keys . fmapMaybe id) $ sequence $ ffor pdMap (\pd -> do
          metas <- _puzzleData_metas pd
          pure $ case Map.null (Map.intersection metas pdMap) of
             False -> Nothing
             True -> Just ())
        qq <- sequence $ (\(i, pd) -> (,,) pd i . Map.keys <$> _puzzleData_metas pd) <$> Map.toAscList pdMap
        let (g , vertexToData, keyToVertex) = graphFromEdges qq
            unfoldingFun :: Graph -> Vertex -> (Vertex, [Vertex])
            unfoldingFun g v = (v, g A.! v)
            notQuiteDFS gr ve = unfoldForest (unfoldingFun gr) ve
            theDFS = traceShowId $ dfs (transposeG g) (fmapMaybe keyToVertex (traceShowId rootElems))

        let forest :: Map PuzzleSortKey (PuzzleData t) = Map.fromList $ zipWith (\k v -> (PuzzleSortKey_Synthetic k, (\(pd, _, _) -> pd) $ vertexToData v)) (negate <$> [1..]) $ (theDFS >>= F.toList)
        pure forest

prunePuzzles
  :: forall t k. (Reflex t, Ord k)
  => Dynamic t PuzzleSelect
  -> Dynamic t (Map k (PuzzleData t))
  -> Dynamic t (Map k (PuzzleData t))
prunePuzzles puzzleSelect puzzleData = join $ prunePuzzlesInner <$> puzzleSelect <*> puzzleData
  where
    prunePuzzlesInner :: PuzzleSelect -> Map k (PuzzleData t) -> Dynamic t (Map k (PuzzleData t))
    prunePuzzlesInner = \case
      PuzzleSelect_All -> constDyn
      PuzzleSelect_And f1 f2 -> \a -> prunePuzzlesInner f1 a >>= prunePuzzlesInner f2
      PuzzleSelect_WithTag tag -> fmap (fmapMaybe id) . sequence . (Map.map $ \pd -> do
         tags <- _puzzleData_tags pd
         pure $ pd <$ Map.lookup tag tags)
      PuzzleSelect_Not f -> \a -> Map.difference a <$> prunePuzzlesInner f a
      PuzzleSelect_HasVoice -> fmap (fmapMaybe id) . sequence . (Map.map $ \pd -> do
         vcs <- _puzzle_voicelink <$> _puzzleData_puzzle pd
         pure $ pd <$ vcs)
      PuzzleSelect_HasMeta meta -> fmap (fmapMaybe id) . sequence . (Map.map $ \pd -> do
         metas <- _puzzleData_metas pd
         pure $ pd <$ Map.lookup meta metas)
      PuzzleSelect_IsMeta -> fmap (fmapMaybe id) . sequence . (Map.map $ \pd -> do
         vcs <- _puzzle_IsMeta <$> _puzzleData_puzzle pd
         pure $ case vcs of 
             True -> Just pd
             False -> Nothing )
      PuzzleSelect_HasSolution -> fmap (fmapMaybe id) . sequence . (Map.map $ \pd -> do
         vcs <- _puzzleData_solutions pd
         pure $ case Map.null vcs of
             True -> Nothing
             False -> Just pd
         )
      PuzzleSelect_HasSolvers -> fmap (fmapMaybe id) . sequence . (Map.map $ \pd -> do
         vcs <- _puzzleData_currentSolvers pd
         pure $ case Map.null vcs of
             True -> Nothing
             False -> Just pd
         )

