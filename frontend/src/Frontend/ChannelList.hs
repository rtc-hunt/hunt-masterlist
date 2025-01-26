module Frontend.ChannelList where

import Control.Monad
import Control.Monad.Fix
import Data.Vessel
import Reflex.Dom.Core
import Obelisk.Route.Frontend
import qualified Data.Map as Map
import Rhyolite.Api hiding (Request)
import Rhyolite.Frontend.App
import Rhyolite.Vessel.Path

import Common.Request
import Common.Route
import Common.View
import Rhyolite.Frontend.Auth.App
import Templates.Partials.ChannelList
import Control.Monad.Ref
import Frontend.Types
import Rhyolite.Vessel.AuthenticatedV

channelSearchResults
  :: ( MonadFix m
     , MonadHold t m
     , AuthenticatedMonadQuery t m
     , AuthReq t m
     , AuthReq t (Client m)
     , DomBuilder t m
     , PostBuild t m
     , SetRoute t (R FrontendRoute) m
     )
  => ChannelList t m
  -> m ()
channelSearchResults cs = do
    mRooms <- maybeDyn <=< watch . ffor (value (_channelList_input cs)) $ \q ->
      privateP ~> key V_Chatrooms ~> key (ChatroomQuery q) ~> semiMapP

    channelClick <- (switchHold never =<<) $ dyn $ ffor mRooms $ \case
      Nothing -> pure never
      Just rooms -> switchDyn . fmap mergeMap <$> list rooms channelItem

    _ <- fmap fanEither . requestingIdentity . ffor (tag (current (value (_channelList_input cs))) (_channelList_add cs)) $ \newName ->
      ApiRequest_Private () $ PrivateRequest_CreateChatroom newName

    setRoute $ fforMaybe channelClick $ \clicks -> ffor (Map.minViewWithKey clicks) $ \((k,_),_) ->
      FrontendRoute_Channel :/ Just k


