{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Backend where


import Prelude hiding ((.))
import Control.Category
import Backend.Request
import Backend.Schema
import Common.Route
import Control.Monad.IO.Class
import Database.PostgreSQL.Simple.Transaction
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Signed
import qualified Data.Map.Monoidal
import qualified Reflex.Query.Class
import qualified Common.View
import Data.Coerce
import Data.Maybe
import Data.Pool
import Data.Signed.ClientSession
import Data.Vessel
import Gargoyle.PostgreSQL.Connect
import Obelisk.Backend
import Obelisk.Route
-- import Rhyolite.Account
import Rhyolite.Backend.App
import Rhyolite.Vessel.AuthMapV
import qualified Web.ClientSession as CS
import System.Environment
import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (catch, SomeException)

import Backend.Listen
import Backend.View
import Backend.Loadtest
import Common.Schema

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
    csk <- CS.getKey "config/backend/clientSessionKey"
    Right cgk <- Aeson.eitherDecodeStrict <$> BS.readFile "config/common/clientGoogleKey"
    let checkToken t = do
          let x = readSignedWithKey @(Id Account) csk t
          pure x
    -- forkIO (addLoad csk)
    withDb "db" $ \pool -> do
      catch (withResource pool $ \conn -> withTransactionSerializable conn $
        runMigrations conn) $ \(e :: SomeException) -> (putStr "Waiting for DB fixes\n" >> threadDelay 100000000)
      (listen, _) <- liftIO $ serveDbOverWebsockets
        (coerce pool)
        (requestHandler pool csk cgk)
        (\nm q -> fmap (fromMaybe emptyV) $ mapDecomposedV (handleAuthMapQuery checkToken (notifyHandler pool nm)) q)
        (QueryHandler $ \q -> fromMaybe emptyV <$> mapDecomposedV (handleAuthMapQuery checkToken (privateQueryHandler pool)) q)
        vesselFromWire
        (vesselPipeline . observePipelineQuery (trackActiveUsers csk pool))
      serve $ \case
        BackendRoute_Listen :/ () -> listen
        BackendRoute_Missing :/ () -> return ()
  , _backend_routeEncoder = fullRouteEncoder
  }


observePipelineQuery :: (q -> IO ()) -> Pipeline IO q q
observePipelineQuery f = Pipeline $ \qh r -> do
  return 
    ( QueryHandler $ \q -> do
        f q
        qr <- runQueryHandler qh q
        return qr
    , Recipient $ \qr -> do
        tellRecipient r qr
    )

