module Backend.View where

import Control.Monad.Logger
import Data.Functor.Identity
import qualified Data.Map.Monoidal as Map
import Data.Pool
import Data.Vessel
import Data.Vessel.SubVessel
import Database.PostgreSQL.Simple
import Rhyolite.Backend.DB

import Backend.View.Chatroom
import Backend.View.Messages

import Common.View

privateQueryHandler
  :: Pool Connection
  -> PrivateChatV Proxy
  -> IO (PrivateChatV Identity)
privateQueryHandler db q = buildV q $ \case
  V_Chatrooms -> \(MapV qs) -> do
    chatrooms <- runNoLoggingT $ runDb (Identity db) $ searchForChatroom $ Map.keysSet qs
    pure $ MapV $ pure <$> chatrooms
  V_Chatroom -> \(MapV cs) -> do
    MapV . fmap pure <$> runNoLoggingT (runDb (Identity db) $ getChatrooms $ Map.keysSet cs)
  V_Messages -> \sv -> do
    rs <- runNoLoggingT $ runDb (Identity db) $
      getMessages . fmap (Map.keysSet . unMapV) . getSubVessel $ sv
    pure $ mkSubVessel . fmap (MapV . fmap Identity) $ rs
