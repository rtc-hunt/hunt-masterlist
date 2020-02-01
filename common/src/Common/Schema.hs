{-# LANGUAGE DeriveGeneric #-}
module Common.Schema where

import Data.Text (Text)
import Data.Time
import Data.Aeson
import GHC.Generics
import Database.Id.Class

data Puzzle = Puzzle
  { _puzzle_Title :: Text
  , _puzzle_URI :: Text
  , _puzzle_SheetURI :: Maybe Text
  , _puzzle_FolderURI :: Text
  , _puzzle_IsMeta :: Bool
  , _puzzle_StartedAt :: Maybe UTCTime
  }
  deriving (Eq, Ord, Show, Generic)
instance HasId Puzzle
instance ToJSON Puzzle
instance FromJSON Puzzle

data Solve = Solve
  { _solve_Puzzle :: Id Puzzle
  , _solve_Solution :: Text
  , _solve_IsBacksolve :: Bool
  }
  deriving (Eq, Ord, Show, Generic)
instance HasId Solve
instance ToJSON Solve
instance FromJSON Solve

data PuzzleMeta = PuzzleMeta
  { _puzzleMeta_Puzzle :: Id Puzzle
  , _puzzleMeta_Meta :: Id Puzzle
  }
  deriving (Eq, Ord, Show, Generic)
instance HasId PuzzleMeta
instance ToJSON PuzzleMeta
instance FromJSON PuzzleMeta

data OpenTime = OpenTime
  { _openTime_Puzzle :: Id Puzzle
  , _openTime_User :: Text -- Change to Id User.
  , _openTime_Start :: UTCTime
  , _openTime_End :: Maybe UTCTime
  }
  deriving (Eq, Ord, Show, Generic)
instance HasId OpenTime
instance ToJSON OpenTime
instance FromJSON OpenTime

data PuzzleTag = PuzzleTag
  { _puzzleTag_Tag :: Text
  , _puzzleTag_Puzzle :: Id Puzzle
  }
  deriving (Eq, Ord, Show, Generic)
instance HasId PuzzleTag
instance ToJSON PuzzleTag
instance FromJSON PuzzleTag

data PuzzleNote = PuzzleNote
  { _puzzleNote_Text :: Text
  , _puzzleNote_Puzzle :: Id Puzzle
  }
  deriving (Eq, Ord, Show, Generic)
instance HasId PuzzleNote
instance ToJSON PuzzleNote
instance FromJSON PuzzleNote

