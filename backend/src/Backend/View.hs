module Backend.View where

import Data.Functor.Identity
import qualified Data.Map.Monoidal as Map
import Data.Pool
import Data.Vessel
import Data.Vessel.SubVessel
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Beam ()

import Backend.Db
import Backend.View.Chatroom
import Backend.View.Messages

import Common.View

privateQueryHandler
  :: Pool Connection
  -> PrivateChatV Proxy
  -> IO (PrivateChatV Identity)
privateQueryHandler db q = buildV q $ \case
  V_Chatrooms -> \(MapV qs) -> do
    chatrooms <- runDb db $ searchForChatroom $ Map.keysSet qs
    pure $ MapV $ pure <$> chatrooms
  V_Chatroom -> \(MapV cs) -> do
    MapV . fmap pure <$> runDb db (getChatrooms $ Map.keysSet cs)
  V_Messages -> \sv -> do
    rs <- runDb db $
      getMessages . fmap (Map.keysSet . unMapV) . getSubVessel $ sv
    pure $ mkSubVessel . fmap (MapV . fmap Identity) $ rs
