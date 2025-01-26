{-# Language OverloadedLists #-}
{-# Language TypeFamilies #-}
{-# Language StandaloneDeriving #-}
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

import Database.Beam.Schema

import Common.Request
import Common.Schema
import Common.View
import Rhyolite.Frontend.Auth.App
import Reflex.Dom.Prerender

type AuthenticatedMonadQuery t m =
   ( MonadQuery t (FullAppAuthV MasterlistApp (Const SelectedCount)) m
   , MonadQuery t (FullAppAuthV MasterlistApp (Const SelectedCount)) (Client m)
   )

type AuthReq t m =
   ( Requester t m
   , Response m ~ Identity
   , Reflex.Request m ~ FullAuthApi MasterlistApp
   )

type UnauthorizedMonadQuery t m = MonadQuery t (FullAppV MasterlistApp (Const SelectedCount))

type MasterlistWidget = FullAppWidget MasterlistApp

data Logout = Logout

-- type ExampleWidget = RhyoliteWidget
--   (AuthMapV AuthToken PrivateChatV (Const SelectedCount))
--   (ApiRequest AuthToken PublicRequest PrivateRequest)

type family HKD f a where
  HKD Identity a = a
  HKD f a = f a

data PuzzleDataT f = PuzzleData
  { _puzzleData_puzzle :: HKD f (Puzzle Identity)
  , _puzzleData_metas :: HKD f (Map (PrimaryKey Puzzle Identity) Text)
  , _puzzleData_tags :: HKD f (Map Text ())
  , _puzzleData_solutions :: HKD f (Map (PrimaryKey Solution Identity) (Solution Identity))
  , _puzzleData_notes :: HKD f (Map (PrimaryKey Note Identity) (Note Identity))
--  , _puzzleData_status :: HKD f [Text]
  , _puzzleData_currentSolvers :: HKD f (Map (PrimaryKey Account Identity) Text)
  }

{- newtype PuzzleDataPatch = PuzzleDataPatch (PuzzleDataT Maybe)

instance Patch PuzzleDataPatch where
  type PatchTarget PuzzleDataPatch = PuzzleDataT Identity
  apply d p = 
-}

type PuzzleDataPatch = Identity (PuzzleDataT Identity)

diffPuzzleData :: PuzzleDataT Identity -> PuzzleDataT Identity -> PuzzleDataPatch
diffPuzzleData pd1 pd2 = Identity pd2

newtype PuzzleOrd = PuzzleOrd { unPuzzleOrd :: PuzzleDataT Identity }

instance Ord PuzzleOrd where
  compare a b = compare (primaryKey $ _puzzleData_puzzle $ unPuzzleOrd a) (primaryKey $ _puzzleData_puzzle $ unPuzzleOrd b)

instance Eq PuzzleOrd where
  a == b = (primaryKey $ _puzzleData_puzzle $ unPuzzleOrd a) == (primaryKey $ _puzzleData_puzzle $ unPuzzleOrd b)

type PuzzleData t = PuzzleDataT (Dynamic t)

deriving instance Eq (PuzzleDataT Identity)
deriving instance Ord (PuzzleDataT Identity)
-- Slightly terrible, but practical?
--instance Eq (PuzzleData t) where
--  a == b = _puzzleData_id a == _puzzleData_id b
--instance Ord (PuzzleDataT Identity) where
--  compare a b = compare (_puzzleData_id a) (_puzzleData_id b)
deriving instance Show (PuzzleDataT Identity)
-- instance Show (PuzzleDataT Identity) where
--   show a = "<PuzzleData For " <> show (_puzzleData_id a) <> ">"

statusTags :: Set Text
statusTags = [ "solved", "in-progress", "stalled", "extraction", "done" ]
