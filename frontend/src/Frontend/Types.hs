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


statusTags :: Set Text
statusTags = [ "solved", "in-progress", "stalled", "extraction", "done" ]
