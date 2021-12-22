module Frontend.Types where

import Data.Functor.Const
import Data.Functor.Identity
import Data.Signed
import Reflex
import Rhyolite.Account
import Rhyolite.Api
import Rhyolite.Frontend.App
import Rhyolite.Vessel.AuthMapV

import Common.Request
import Common.View

data Logout = Logout

type ExampleCredential = Signed (AuthToken Identity)
type ExampleWidget = RhyoliteWidget
  (AuthMapV ExampleCredential PrivateChatV (Const SelectedCount))
  (ApiRequest ExampleCredential PublicRequest PrivateRequest)
