{-# OPTIONS_GHC -Werror=incomplete-patterns #-}
{-# Language LambdaCase #-}

module Frontend.SortSelect where

import Control.Monad
import Reflex
import qualified Data.Array as A
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Graph
import Data.Tree
import Data.Foldable as F

import Common.Route

import Frontend.Types
import Common.Schema
import Data.Functor.Identity
import Data.Maybe

import Data.Array ((!))

import Debug.Trace

toSortKeys
  :: PuzzleOrdering
  -> Map (Id Puzzle) (PuzzleDataT Identity)
  -> Map (PuzzleSortKey) (PuzzleDataT Identity)
toSortKeys puzzleOrdering puzzleData = toSortKeysInner puzzleOrdering puzzleData
  where
    toSortKeysInner :: PuzzleOrdering -> Map (Id Puzzle) (PuzzleDataT Identity) -> Map PuzzleSortKey (PuzzleDataT Identity)
    toSortKeysInner = \case
      PuzzleOrdering_Any -> Map.mapKeys PuzzleSortKey_Id
      PuzzleOrdering_ByMeta -> \pdMap ->
        let
          -- First get a topo-sort-ish ordering (strongly connected components ordered in topological order) to use as a bias for depth-first search
          rekey_pre k = (_puzzle_IsMeta $ _puzzleData_puzzle $ pdMap Map.! k, k)
          qq_pre = (\(i, pd) -> let pks = Map.keys $ _puzzleData_metas pd in (pd, rekey_pre i, rekey_pre <$> pks)) <$> Map.toAscList pdMap
          (g_pre , vertexToData_pre, keyToVertex_pre) = graphFromEdges qq_pre

          -- Actually compute the components, using meta->puzzle edges:
          pre_scc_order = Map.fromList $ flip zip [(1 :: Int)..] $ (\(_,k,_) -> snd k) . vertexToData_pre <$> ((scc $ transposeG g_pre) >>= flatten)

          -- and make a remapping function for keys:
          keyremap k = pre_scc_order Map.! k

          -- then we use that ordering to make new keys, and build up our meta graph again:
          -- keyremap k = let v = pdMap Map.! k in (_puzzle_IsMeta $ _puzzleData_puzzle v, negate . Map.size . _puzzleData_metas $ v, k)
          qq = (\(i, pd) -> let pks = Map.keys $ _puzzleData_metas pd in (pd, keyremap i, keyremap <$> pks)) <$> Map.toAscList pdMap
          (g , vertexToData, keyToVertex) = graphFromEdges qq
          -- get connected components again with the new vertex numbering
          gscc = scc g
          -- find all components that have no out-edges in puzzle -> meta and pick an element of each to use as a root
          rootComponents = rootLabel <$> ffilter (\comp -> all (\v -> all (`elem` comp) (g ! v)) comp) gscc
          rootElems = (\(_, a, _) -> a) . vertexToData <$> rootComponents

          -- unfoldingFun :: Graph -> Vertex -> (Vertex, [Vertex])
          -- unfoldingFun g v = (v, g A.! v)
          -- notQuiteDFS gr ve = unfoldForest (unfoldingFun gr) ve
          
          -- and do a depth-first search on the graph, using meta->puzzle edges:
          theDFS = dfs (transposeG g) (fmapMaybe keyToVertex (rootElems))
          
          -- and zip it up with a number key, for the UI to use ordering:
          sorted :: Map PuzzleSortKey (PuzzleDataT Identity) = Map.fromList $ zipWith (\k v -> (PuzzleSortKey_Synthetic k, (\(pd, _, _) -> pd) $ vertexToData v)) (negate <$> [1..]) $ (theDFS >>= F.toList)
        in sorted

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

shouldShowPuzzle ::
  PuzzleSelect -> PuzzleDataT Identity -> Bool
shouldShowPuzzle puzzleSelect puzzleData = prunePuzzlesInner puzzleSelect puzzleData
  where
    prunePuzzlesInner :: PuzzleSelect -> PuzzleDataT Identity -> Bool
    prunePuzzlesInner = \case
      PuzzleSelect_All -> const True
      PuzzleSelect_And f1 f2 -> \a -> prunePuzzlesInner f2 a && prunePuzzlesInner f1 a
      PuzzleSelect_WithTag tag -> Map.member tag . _puzzleData_tags
      PuzzleSelect_Not f -> not . prunePuzzlesInner f
      PuzzleSelect_HasVoice -> isJust . _puzzle_voicelink . _puzzleData_puzzle
      PuzzleSelect_HasMeta meta -> \p -> Map.member meta (_puzzleData_metas p) || (meta == (PuzzleId $ _puzzle_id $ _puzzleData_puzzle p))
      PuzzleSelect_IsMeta -> _puzzle_IsMeta . _puzzleData_puzzle
      PuzzleSelect_HasSolution -> not . Map.null . _puzzleData_solutions
      PuzzleSelect_HasSolvers -> not . Map.null . _puzzleData_currentSolvers

{-
includePuzzle
  :: forall t k. (Reflex t)
  => PuzzleSelect
  -> PuzzleData t
  -> Bool
includePuzzle puzzleSelect puzzleData = join $ prunePuzzlesInner <$> puzzleSelect <*> puzzleData
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
-}
