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

import Common.Route

import Frontend.Types
import Common.Schema
import Data.Functor.Identity

import Debug.Trace

toSortKeys
  :: forall t. (Reflex t)
  => Dynamic t PuzzleOrdering
  -> Dynamic t (Map (Id Puzzle) (PuzzleDataT Identity))
  -> Dynamic t (Map (PuzzleSortKey) (PuzzleDataT Identity))
toSortKeys puzzleOrdering puzzleData = toSortKeysInner <$> puzzleOrdering <*> puzzleData
  where
    toSortKeysInner :: PuzzleOrdering -> Map (Id Puzzle) (PuzzleDataT Identity) -> Map PuzzleSortKey (PuzzleDataT Identity)
    toSortKeysInner = \case
      PuzzleOrdering_Any -> Map.mapKeys PuzzleSortKey_Id
      PuzzleOrdering_ByMeta -> \pdMap ->
        let
          rootElems = Map.keys $ fmapMaybe id $ ffor pdMap $ \pd ->
                        case (Map.null (Map.intersection (_puzzleData_metas pd) pdMap)) of
                            False -> Nothing
                            True -> Just ()
          qq = (\(i, pd) -> (pd, i, Map.keys $ _puzzleData_metas pd)) <$> Map.toAscList pdMap
          (g , vertexToData, keyToVertex) = graphFromEdges qq
          unfoldingFun :: Graph -> Vertex -> (Vertex, [Vertex])
          unfoldingFun g v = (v, g A.! v)
          notQuiteDFS gr ve = unfoldForest (unfoldingFun gr) ve
          theDFS = traceShowId $ dfs (transposeG g) (fmapMaybe keyToVertex (traceShowId rootElems))
          forest :: Map PuzzleSortKey (PuzzleDataT Identity) = Map.fromList $ zipWith (\k v -> (PuzzleSortKey_Synthetic k, (\(pd, _, _) -> pd) $ vertexToData v)) (negate <$> [1..]) $ (theDFS >>= F.toList)
        in forest

prunePuzzles
  :: forall t k. (Reflex t, Ord k)
  => Dynamic t PuzzleSelect
  -> Dynamic t (Map k (PuzzleDataT Identity))
  -> Dynamic t (Map k (PuzzleDataT Identity))
prunePuzzles puzzleSelect puzzleData = prunePuzzlesInner <$> puzzleSelect <*> puzzleData
  where
    prunePuzzlesInner :: PuzzleSelect -> Map k (PuzzleDataT Identity) -> Map k (PuzzleDataT Identity)
    prunePuzzlesInner = \case
      PuzzleSelect_All -> id
      PuzzleSelect_And f1 f2 -> \a -> prunePuzzlesInner f2 $ prunePuzzlesInner f1 a
      PuzzleSelect_WithTag tag -> fmap (fmapMaybe id) $ (Map.map $ \pd -> pd <$ Map.lookup tag (_puzzleData_tags pd))
      PuzzleSelect_Not f -> \a -> Map.difference a $ prunePuzzlesInner f a
      PuzzleSelect_HasVoice -> fmap (fmapMaybe id) $ (Map.map $ \pd -> pd <$ (_puzzle_voicelink $ _puzzleData_puzzle pd))
      PuzzleSelect_HasMeta meta -> fmap (fmapMaybe id) $ (Map.map $ \pd -> pd <$ Map.lookup meta (_puzzleData_metas pd))
      PuzzleSelect_IsMeta -> fmap (fmapMaybe id) $ (Map.map $ \pd -> case (_puzzle_IsMeta $ _puzzleData_puzzle pd) of
             True -> Just pd
             False -> Nothing)
      PuzzleSelect_HasSolution -> fmap (fmapMaybe id) $ (Map.map $ \pd -> case (Map.null $ _puzzleData_solutions pd) of
             True -> Nothing
             False -> Just pd)
      PuzzleSelect_HasSolvers -> fmap (fmapMaybe id) $ (Map.map $ \pd -> case (Map.null $ _puzzleData_currentSolvers pd) of
             True -> Nothing
             False -> Just pd)
