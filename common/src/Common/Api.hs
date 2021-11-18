module Common.Api where

import Data.Aeson
import Data.Aeson.GADT.TH
import Data.Constraint.Extras.TH
import Data.Functor.Identity
import Data.Text
import Rhyolite.Sign

import Common.Schema

data PrivateRequest a where
  PrivateRequest_SendMessage :: Id Chatroom -> Text -> PrivateRequest (Either Text ())
  PrivateRequest_CreateChatroom :: Text -> PrivateRequest (Either Text (Id Chatroom))

data PublicRequest a where
  PublicRequest_Login :: Text -> Text -> PublicRequest (Either Text (Signed (AuthToken Identity)))
  PublicRequest_SignUp :: Text -> Text -> PublicRequest (Either Text (Signed (AuthToken Identity)))

deriveArgDict ''PrivateRequest
deriveJSONGADT ''PrivateRequest
deriveArgDict ''PublicRequest
deriveJSONGADT ''PublicRequest
