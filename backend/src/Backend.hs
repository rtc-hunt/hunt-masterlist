{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Backend where

import Backend.Request
import Backend.Schema
import Common.Route
import Control.Monad.IO.Class
import Database.PostgreSQL.Simple.Transaction
import Data.Coerce
import Data.Maybe
import Data.Pool
import Data.Signed.ClientSession
import Data.Vessel
import Gargoyle.PostgreSQL.Connect
import Obelisk.Backend
import Obelisk.Route
import Rhyolite.Account
import Rhyolite.Backend.App
import Rhyolite.Vessel.AuthMapV
import qualified Web.ClientSession as CS

import Backend.Listen
import Backend.View
import Common.Schema

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
    csk <- CS.getKey "config/backend/clientSessionKey"
    let checkToken t = do
          let x = readSignedWithKey @(Id Account) csk t
          pure x
    withDb "db" $ \pool -> do
      withResource pool $ \conn -> withTransactionSerializable conn $
        runMigrations conn
      (listen, _) <- liftIO $ serveDbOverWebsockets
        (coerce pool)
        (requestHandler pool csk)
        (\nm q -> fmap (fromMaybe emptyV) $ mapDecomposedV (handleAuthMapQuery checkToken (notifyHandler pool nm)) q)
        (QueryHandler $ \q -> fromMaybe emptyV <$> mapDecomposedV (handleAuthMapQuery checkToken (privateQueryHandler pool)) q)
        vesselFromWire
        vesselPipeline
      serve $ \case
        BackendRoute_Listen :/ () -> listen
        BackendRoute_Missing :/ () -> return ()
  , _backend_routeEncoder = fullRouteEncoder
  }
