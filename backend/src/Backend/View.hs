module Backend.View where

import Control.Monad.Logger
import Data.Functor.Identity
import qualified Data.Map.Monoidal as Map
import Data.Pool
import Data.Vessel
import Database.PostgreSQL.Simple
import Rhyolite.Backend.DB

import Backend.Query
import Common.View

queryHandler :: Pool Connection -> ChatV Proxy -> IO (ChatV Identity)
queryHandler db q = buildV q $ \case
  V_Chatrooms -> \(MapV qs) -> do
    chatrooms <- runNoLoggingT $ runDb (Identity db) $ searchForChatroom $ Map.keysSet qs
    pure $ MapV $ Identity <$> chatrooms
  V_Chatroom -> \(MapV cs) -> do
    MapV . fmap Identity <$> runNoLoggingT (runDb (Identity db) $ getChatrooms $ Map.keysSet cs)
  V_Messages -> \(MapV cs) -> do
    MapV . fmap Identity <$> runNoLoggingT (runDb (Identity db) $ getMessages $ Map.keysSet cs)
