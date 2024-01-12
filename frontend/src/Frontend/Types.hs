{-# Language OverloadedLists #-}
{-# Language TypeFamilies #-}
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

type family HKD f a where
  HKD Identity a = a
  HKD f a = f a

data PuzzleDataT f = PuzzleData
  { _puzzleData_puzzle :: HKD f (Puzzle Identity)
  , _puzzleData_metas :: HKD f (Map (PrimaryKey Puzzle Identity) Text)
  , _puzzleData_tags :: HKD f (Map Text ())
  , _puzzleData_solutions :: HKD f (Map (PrimaryKey Solution Identity) (Solution Identity))
  , _puzzleData_notes :: HKD f (Map (PrimaryKey Note Identity) (Note Identity))
  , _puzzleData_status :: HKD f [Text]
  , _puzzleData_currentSolvers :: HKD f (Map (PrimaryKey Account Identity) Text)
  }

type PuzzleData t = PuzzleDataT (Dynamic t)

statusTags :: Set Text
statusTags = [ "solved", "in-progress", "stalled", "extraction", "done" ]
