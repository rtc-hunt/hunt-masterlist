{-# OPTIONS_GHC -Werror=incomplete-patterns #-}
module Frontend.SortSelect where

import Reflex
import Data.Map as Map

import Frontend.Types
import Common.Schema


toSortKeys
  :: (Reflex t)
  => Dynamic t PuzzleOrdering
  -> Dynamic t (Map (Id Puzzle) (PuzzleData t))
  -> Dynamic t (Map (PuzzleSortKey) (PuzzleData t))
toSortKeys _ = fmap $ Map.mapKeys PuzzleSortKey_Id

prunePuzzles
  :: (Reflex t)
  => Dynamic t PuzzleSelect
  -> Dynamic t (Map k (PuzzleData t))
  -> Dynamic t (Map k (PuzzleData t))
prunePuzzles _ = id

