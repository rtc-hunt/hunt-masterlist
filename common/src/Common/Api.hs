module Common.Api where

import Data.Aeson
import Data.Aeson.GADT.TH
import Data.Constraint.Extras.TH
import Data.Functor.Identity
import Data.Text
import Rhyolite.Sign

import Common.Schema

commonStuff :: String
commonStuff = "Here is a string defined in Common.Api"

data PrivateRequest a where
  PrivateRequest_SendMessage :: Id Chatroom -> Text -> PrivateRequest (Either Text ())

data PublicRequest a where
  PublicRequest_Login :: Text -> Text -> PublicRequest (Either Text (Signed (AuthToken Identity)))

data ExampleRequest token a where
  ExampleRequest_Private :: token -> PrivateRequest a -> ExampleRequest token a
  ExampleRequest_Public :: PublicRequest a -> ExampleRequest token a

deriveArgDict ''PrivateRequest
deriveJSONGADT ''PrivateRequest
deriveArgDict ''PublicRequest
deriveJSONGADT ''PublicRequest
deriveArgDict ''ExampleRequest
deriveJSONGADT ''ExampleRequest
