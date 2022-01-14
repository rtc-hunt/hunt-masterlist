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
  deriving (Eq, Show, Ord)

instance Semigroup PuzzleQuery where
  PuzzleQuery sa oa <> PuzzleQuery sb ob = PuzzleQuery (sa <> sb) (oa <> ob)

instance Monoid PuzzleQuery where
  mempty = PuzzleQuery PuzzleSelect_All PuzzleOrdering_Any

data PuzzleSelect
  = PuzzleSelect_All
  deriving (Eq, Show, Ord)

instance Semigroup PuzzleSelect where
  PuzzleSelect_All <> PuzzleSelect_All = PuzzleSelect_All

instance Monoid PuzzleSelect where
  mempty = PuzzleSelect_All

data PuzzleOrdering
  = PuzzleOrdering_Any
  deriving (Eq, Show, Ord)

instance Semigroup PuzzleOrdering where
  PuzzleOrdering_Any <> PuzzleOrdering_Any = PuzzleOrdering_Any

instance Monoid PuzzleOrdering where
  mempty = PuzzleOrdering_Any

data PuzzleSortKey
  = PuzzleSortKey_Id (Id Puzzle)
  deriving (Eq, Show, Ord)
