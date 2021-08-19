module Backend where

import Backend.Request
import Backend.Schema
import Common.Route
import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Functor.Identity
import Database.Groundhog (runMigration)
import Database.Groundhog.Generic.Migration (getTableAnalysis)
import Database.Groundhog.Postgresql
import Gargoyle.PostgreSQL.Connect
import Obelisk.Backend
import Obelisk.Route
import Rhyolite.Backend.Account
import Rhyolite.Backend.App
import Rhyolite.Backend.DB
import qualified Web.ClientSession as CS

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
    csk <- CS.getKey "config/backend/clientSessionKey"
    withDb "db" $ \db -> runNoLoggingT $ do
      runDb (Identity db) $ do
        tables <- getTableAnalysis
        runMigration $ do
          migrateAccount tables
          migrateSchema tables
      -- liftIO $ serveDbOverWebsockets (Postgresql <$> db) (requestHandler db csk) _ _ _ _
      return ()
    serve $ \case
      BackendRoute_Listen :/ () -> return ()
      BackendRoute_Missing :/ () -> return ()
  , _backend_routeEncoder = fullRouteEncoder
  }
