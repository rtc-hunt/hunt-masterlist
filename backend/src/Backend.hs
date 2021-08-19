module Backend where

import Backend.Schema
import Common.Route
import Control.Monad.Logger
import Data.Functor.Identity
import Database.Groundhog (runMigration)
import Database.Groundhog.Generic.Migration (getTableAnalysis)
import Gargoyle.PostgreSQL.Connect
import Obelisk.Backend
import Obelisk.Route
import Rhyolite.Backend.Account
import Rhyolite.Backend.DB

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
    withDb "db" $ \db -> runNoLoggingT $ do
      runDb (Identity db) $ do
        tables <- getTableAnalysis
        runMigration $ do
          migrateAccount tables
          migrateSchema tables
      return ()
    serve $ \case
      BackendRoute_Listen :/ () -> return ()
      BackendRoute_Missing :/ () -> return ()
  , _backend_routeEncoder = fullRouteEncoder
  }
