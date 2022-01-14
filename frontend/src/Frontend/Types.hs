{-# Language OverloadedLists #-}
module Frontend.Types where

import Data.Functor.Const
import Reflex
import Rhyolite.Api
import Rhyolite.Frontend.App
import Rhyolite.Vessel.AuthMapV
import Data.Text
import Data.Map (Map)
import Data.Map as Map
import Control.Monad.Identity
import Data.Set

import Common.Request
import Common.Schema
import Common.View

data Logout = Logout

type ExampleWidget = RhyoliteWidget
  (AuthMapV AuthToken PrivateChatV (Const SelectedCount))
  (ApiRequest AuthToken PublicRequest PrivateRequest)

data PuzzleData t = PuzzleData
  { _puzzleData_puzzle :: Dynamic t (Puzzle Identity)
  , _puzzleData_metas :: Dynamic t (Map (PrimaryKey Puzzle Identity) Text)
  , _puzzleData_tags :: Dynamic t (Map Text ())
  , _puzzleData_solutions :: Dynamic t (Map (PrimaryKey Solution Identity) (Solution Identity))
  , _puzzleData_notes :: Dynamic t (Map (PrimaryKey Note Identity) (Note Identity))
  , _puzzleData_status :: Dynamic t Text
  , _puzzleData_currentSolvers :: Dynamic t (Map (PrimaryKey Account Identity) Text)
  }

data PuzzleQuery = PuzzleQuery 
  { _puzzleQuery_select :: PuzzleSelect
  , _puzzleQuery_ordering :: PuzzleOrdering
  }
  deriving (Eq, Show, Ord, Read)

instance Semigroup PuzzleQuery where
  PuzzleQuery sa oa <> PuzzleQuery sb ob = PuzzleQuery (sa <> sb) (oa <> ob)

instance Monoid PuzzleQuery where
  mempty = PuzzleQuery PuzzleSelect_All PuzzleOrdering_Any

data PuzzleSelect
  = PuzzleSelect_All
  | PuzzleSelect_And PuzzleSelect PuzzleSelect
  | PuzzleSelect_Not PuzzleSelect
  | PuzzleSelect_WithTag Text
  | PuzzleSelect_HasVoice
  | PuzzleSelect_IsMeta
  | PuzzleSelect_HasSolution
  | PuzzleSelect_HasSolvers
  | PuzzleSelect_HasMeta (Id Puzzle)
  deriving (Eq, Show, Ord, Read)

instance Semigroup PuzzleSelect where
  PuzzleSelect_All <> PuzzleSelect_All = PuzzleSelect_All
  PuzzleSelect_All <> a = a
  a <> PuzzleSelect_All = a
  a <> b = PuzzleSelect_And a b

instance Monoid PuzzleSelect where
  mempty = PuzzleSelect_All

data PuzzleOrdering
  = PuzzleOrdering_Any
  | PuzzleOrdering_ByMeta
  deriving (Eq, Show, Ord, Read)

instance Semigroup PuzzleOrdering where
  PuzzleOrdering_Any <> PuzzleOrdering_Any = PuzzleOrdering_Any
  a <> PuzzleOrdering_Any = a
  PuzzleOrdering_Any <> a = a

instance Monoid PuzzleOrdering where
  mempty = PuzzleOrdering_Any

data PuzzleSortKey
  = PuzzleSortKey_Id (Id Puzzle)
  | PuzzleSortKey_Synthetic Int
  deriving (Eq, Show, Ord)

statusTags :: Set Text
statusTags = [ "solved", "in-progress", "stalled", "extraction", "done" ]
