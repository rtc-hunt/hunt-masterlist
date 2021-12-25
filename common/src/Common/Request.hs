module Common.Request where

import Data.Aeson
import Data.Aeson.GADT.TH
import Data.Constraint.Extras.TH
import Data.Text

import Common.Schema

-- Private requests require the user to be logged in.
data PrivateRequest a where
  PrivateRequest_SendMessage :: Id Chatroom -> Text -> PrivateRequest (Either Text ())
  PrivateRequest_CreateChatroom :: Text -> PrivateRequest (Either Text (Id Chatroom))
  PrivateRequest_LatestMessage :: Id Chatroom -> PrivateRequest (Either Text (Id Chatroom, Int))

-- Public requests are those which do not require the user to already be logged in.
data PublicRequest a where
  PublicRequest_Login :: Text -> Text -> PublicRequest (Either Text AuthToken)
  PublicRequest_Signup :: Text -> Text -> PublicRequest (Either Text AuthToken)

deriveArgDict ''PrivateRequest
deriveJSONGADT ''PrivateRequest
deriveArgDict ''PublicRequest
deriveJSONGADT ''PublicRequest
