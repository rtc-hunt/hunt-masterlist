module Common.Api where

import Data.Aeson
import Data.Aeson.GADT.TH
import Data.Constraint.Extras.TH
import Data.Functor.Identity
import Data.Text
import Database.Id.Class
import Rhyolite.Account
import Rhyolite.Sign

import Common.Schema

commonStuff :: String
commonStuff = "Here is a string defined in Common.Api"

data PrivateRequest a where
  PrivateRequest_SendMessage :: Id Chatroom -> Text -> PrivateRequest (Either Text ())

data PublicRequest a where
  PublicRequest_Login :: Text -> Text -> PublicRequest (Either Text (Signed (AuthToken Identity)))

data Request token a where
  Request_Private :: token -> PrivateRequest a -> Request token a
  Request_Public :: PublicRequest a -> Request token a

deriveArgDict ''PrivateRequest
deriveJSONGADT ''PrivateRequest
deriveArgDict ''PublicRequest
deriveJSONGADT ''PublicRequest
deriveArgDict ''Request
deriveJSONGADT ''Request
