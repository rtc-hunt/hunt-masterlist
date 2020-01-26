{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Backend where

import Common.Route
import Backend.Schema
import Obelisk.Backend
import qualified Data.Map as Map
import Control.Monad.Logger
import Rhyolite.Backend.Logging
import Obelisk.ExecutableConfig.Lookup
import Control.Monad.IO.Class
import Database.Groundhog (runMigration)
import Database.Groundhog.Generic.Migration hiding (migrateSchema)
import Gargoyle.PostgreSQL.Connect
import Control.Monad.Identity
import Rhyolite.Backend.DB
import Rhyolite.Backend.App
import Common.App
import Backend.ViewSelectorHandler
import Data.Dependent.Sum (DSum(..))

import Reflex.Query.Class
import Data.Aeson
import GHC.Generics


backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
       _configs <- getConfigs
       withLogging [LoggingConfig (RhyoliteLogAppender_Stderr $ RhyoliteLogAppenderStderr Nothing) $ Just (Map.singleton "TOKEN" RhyoliteLogLevel_Debug)] $ do
         logger <- fmap LoggingEnv askLoggerIO
         liftIO $ withDb "db" $ \db -> do
           _ <- runLoggingEnv logger $
             runDb (Identity db) $ do
               tableInfo <- getTableAnalysis
               runMigration $ do
                 migrateSchema tableInfo
           (listen, _) <- liftIO $ serveDbOverWebsockets (convertPostgresPool db)
             (handleRequests)
             (notifyHandler )
             (viewSelectorHandler logger db)
             functorFromWire
             standardPipeline
           serve $ \case
             BackendRoute_Missing :=> _ -> return ()
             BackendRoute_Listen :=> Identity () -> listen
  , _backend_routeEncoder = fullRouteEncoder
  }


handleRequests :: RequestHandler HMLRequest IO
handleRequests = RequestHandler $ \case
  HMLRequest_None -> return ()

data Notification = Notification
  deriving (Generic)

instance FromJSON Notification
instance ToJSON Notification

notifyHandler :: Notification -> (HMLViewSelector a) -> IO (QueryResult (HMLViewSelector a))
notifyHandler = undefined
