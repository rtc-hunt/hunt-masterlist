module Common.Request where

import Data.Aeson
import Data.Aeson.GADT.TH
import Data.Constraint.Extras.TH
import Data.Functor.Identity
import Data.Text
import Rhyolite.Sign

import Common.Schema

-- Private requests require the user to be logged in.
data PrivateRequest a where
  PrivateRequest_SendMessage :: Id Chatroom -> Text -> PrivateRequest (Either Text ())
  PrivateRequest_CreateChatroom :: Text -> PrivateRequest (Either Text (Id Chatroom))
  PrivateRequest_LatestMessage :: Id Chatroom -> PrivateRequest (Either Text (Id Chatroom, Int))

-- Public requests are those which do not require the user to already be logged in.
data PublicRequest a where
  PublicRequest_Login :: Text -> Text -> PublicRequest (Either Text (Signed (AuthToken Identity)))
  PublicRequest_SignUp :: Text -> Text -> PublicRequest (Either Text (Signed (AuthToken Identity)))

deriveArgDict ''PrivateRequest
deriveJSONGADT ''PrivateRequest
deriveArgDict ''PublicRequest
deriveJSONGADT ''PublicRequest
