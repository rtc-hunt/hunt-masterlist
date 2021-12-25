module Frontend.Types where

import Data.Functor.Const
import Reflex
import Rhyolite.Api
import Rhyolite.Frontend.App
import Rhyolite.Vessel.AuthMapV

import Common.Request
import Common.Schema
import Common.View

data Logout = Logout

type ExampleWidget = RhyoliteWidget
  (AuthMapV AuthToken PrivateChatV (Const SelectedCount))
  (ApiRequest AuthToken PublicRequest PrivateRequest)
