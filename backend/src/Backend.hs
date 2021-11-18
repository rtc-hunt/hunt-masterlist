{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Backend where

import Backend.Request
import Backend.Schema
import Common.Route
import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Coerce
import Data.Functor.Identity
import Data.Maybe
import Data.Vessel
import Database.Groundhog (runMigration)
import Database.Groundhog.Generic.Migration (getTableAnalysis)
import Database.Groundhog.Postgresql
import Gargoyle.PostgreSQL.Connect
import Obelisk.Backend
import Obelisk.Route
import Rhyolite.Account
import Rhyolite.Backend.Account
import Rhyolite.Backend.App
import Rhyolite.Backend.DB
import Rhyolite.Backend.Sign
import Rhyolite.Vessel.AuthMapV
import qualified Web.ClientSession as CS

import Backend.Listen
import Backend.View

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
    csk <- CS.getKey "config/backend/clientSessionKey"
    let checkToken t = do
          let x = readSignedWithKey @(AuthToken Identity) csk t
          pure x
    withDb "db" $ \db -> do
      runNoLoggingT $ runDb (Identity db) $ do
        tables <- getTableAnalysis
        runMigration $ do
          migrateAccount tables
          migrateSchema tables
      (listen, _) <- liftIO $ serveDbOverWebsockets
        (coerce db)
        (requestHandler db csk)
        (\nm q -> fmap (fromMaybe emptyV) $ mapDecomposedV (handleAuthMapQuery checkToken (notifyHandler db nm)) q)
        (QueryHandler $ \q -> fromMaybe emptyV <$> mapDecomposedV (handleAuthMapQuery checkToken (privateQueryHandler db)) q)
        vesselFromWire
        vesselPipeline
      serve $ \case
        BackendRoute_Listen :/ () -> listen
        BackendRoute_Missing :/ () -> return ()
  , _backend_routeEncoder = fullRouteEncoder
  }
