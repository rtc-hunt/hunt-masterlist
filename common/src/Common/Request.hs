module Common.Request where

import Data.Aeson
import Data.Aeson.GADT.TH
import Data.Constraint.Extras.TH
import Data.Text
import Data.Functor.Identity

import Common.Schema

-- Private requests require the user to be logged in.
data PrivateRequest a where
  PrivateRequest_SendMessage :: Id Chatroom -> Text -> PrivateRequest (Either Text ())
  PrivateRequest_CreateChatroom :: Text -> PrivateRequest (Either Text (Id Chatroom))
  PrivateRequest_LatestMessage :: Id Chatroom -> PrivateRequest (Either Text (Id Chatroom, Int))
  PrivateRequest_AddPuzzle :: Text -> Bool -> Text -> Id Hunt -> PrivateRequest (Either Text (Id Puzzle))
  PrivateRequest_PuzzleCommand :: PuzzleCommand a -> PrivateRequest (Either Text ())
  PrivateRequest_UpdatePuzzle :: Puzzle Identity -> PrivateRequest (Either Text ())
  PrivateRequest_Renick :: Text -> PrivateRequest (Either Text ())

data PuzzleCommand a where
  PuzzleCommand_Tag :: Id Puzzle -> Text -> PuzzleCommand (Id Puzzle, Text)
  PuzzleCommand_Untag :: Id Puzzle -> Text -> PuzzleCommand (Id Puzzle, Text)
  PuzzleCommand_Note :: Id Puzzle -> Text -> PuzzleCommand (Id Puzzle, Text)
  PuzzleCommand_Solve :: Id Puzzle -> Text -> Bool -> PuzzleCommand (Id Puzzle, Text, Bool)
  PuzzleCommand_UnSolve :: Id Solution -> PuzzleCommand (Id Solution)
  PuzzleCommand_AddMeta :: Id Puzzle -> Id Puzzle -> PuzzleCommand (Id Puzzle, Id Puzzle)
  PuzzleCommand_RemoveMeta :: Id Metapuzzle -> PuzzleCommand (Id Metapuzzle)

-- Public requests are those which do not require the user to already be logged in.
data PublicRequest a where
  PublicRequest_GoogleLogin :: Text -> PublicRequest (Either Text AuthToken)
  -- PublicRequest_Login :: Text -> Text -> PublicRequest (Either Text AuthToken)
  -- PublicRequest_Signup :: Text -> Text -> PublicRequest (Either Text AuthToken)

deriveArgDict ''PuzzleCommand
deriveJSONGADT ''PuzzleCommand

deriveArgDict ''PrivateRequest
deriveJSONGADT ''PrivateRequest
deriveArgDict ''PublicRequest
deriveJSONGADT ''PublicRequest
