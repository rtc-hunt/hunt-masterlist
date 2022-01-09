{-# Language TypeApplications #-}
module Backend.Loadtest where

import Rhyolite.Frontend.App
import Rhyolite.WebSocket
import Obelisk.Route
import Obelisk.Route.Frontend
import Common.Route
import Network.WebSockets.Client
import Network.WebSockets
import Common.View
import Common.Schema
import Reflex.Query.Class
import Web.ClientSession as CS
import Control.Exception

import Rhyolite.Backend.WebSocket
import Rhyolite.Backend.App

import qualified Data.Aeson as Aeson
import Rhyolite.Vessel.AuthMapV
import Rhyolite.Vessel.ErrorV
import Rhyolite.Api
import Rhyolite.Frontend.App
import Data.Signed.ClientSession

import Common.Request

import Data.Vessel 
import Control.Monad
import Control.Concurrent

backWatch
  ::
  forall a r b.
  (-- Reflex t
  -- , MonadQuery t (v (Const SelectedCount)) m
  -- v ~ PrivateChatV
  -- (v (Const SelectedCount)) ~ (AuthMapV AuthToken PrivateChatV (Const SelectedCount))
  r ~ ApiRequest AuthToken PublicRequest PrivateRequest
--  , QueryResult (PrivateChatV (Const ())) ~ PrivateChatV Identity
  -- , MonadHold t m
  -- , MonadFix m
--  , Eq (PrivateChatV Identity)
  )
  => AuthToken
  -> Connection
  -> FullPath a PrivateChatV b
  -> IO () 
backWatch token conn path =
  sendEncodedDataMessage conn $ (id @(WebSocketRequest (AuthMapV AuthToken PrivateChatV (Const ())) r)) $ WebSocketRequest_ViewSelector $ mapV (const (Const ())) $ _queryMorphism_mapQuery (authMapQueryMorphism token) $ liftErrorV $ _path_to path (Const 1)

addLoad :: CS.Key -> IO ()
addLoad csk = void $ forkIO $ (threadDelay 3000000 >>) $ flip catch (\(e :: SomeException) -> print ("ERROR: DYING", e)) $ do
  token <- signWithKey csk (AccountId 1)
  let someClient = forkIO $ runClient "127.0.0.1" 8000 "/listen" $ \conn -> do
        backWatch token conn $ key V_HuntPuzzles ~> key ((HuntId 1) :: Id Hunt)
        backWatch token conn $ key V_Messages ~> key ((ChatroomId 17) :: Id Chatroom) ~> key (RequestInterval 1 1 10000)
        forever $ (receiveDataMessage conn >>= print)
  sequenceA [ someClient | a <- [0..200]]
  let someClientBeater = (threadDelay 10000 >>) $ forkIO $ runClient "127.0.0.1" 8000 "/listen" $ \conn -> do
        backWatch token conn $ key V_HuntPuzzles ~> key ((HuntId 1) :: Id Hunt)
        backWatch token conn $ key V_Messages ~> key ((ChatroomId 17) :: Id Chatroom) ~> key (RequestInterval 1 1 10000)
        threadDelay 1000000
  sequenceA [ someClientBeater | a <- [0..20000]]
--      forkIO $ forever $ print "tick" >> receiveDataMessage conn >>= print
  pure ()
