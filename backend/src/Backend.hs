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
import Data.IORef
import Data.Vessel
import Gargoyle.PostgreSQL.Connect
import Obelisk.Backend
import Obelisk.Route
-- import Rhyolite.Account
import Rhyolite.Backend.App
import Rhyolite.Vessel.AuthMapV
import qualified Web.ClientSession as CS
import System.Environment
import System.Directory
import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (catch, SomeException, handle, try)

import Backend.Listen
import Backend.View
import Backend.Loadtest
import Common.Schema
import Debug.Trace


createCSK = do
  (bytes, key) <- CS.randomKey
  BS.writeFile "config/backend/clientSessionKey" $ bytes
  return key

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
    csk <- handle (\(_ :: SomeException) -> putStr "No config/backend/clientSessionKey, using and saving a random key" >> createCSK) $ CS.getKey "config/backend/clientSessionKey"
    cgkr <- (fmap Aeson.eitherDecodeStrict) <$> try @SomeException (BS.readFile "config/common/clientGoogleKey")
    cgk <- case cgkr of
          Right (Right cgk) -> return cgk
          _ -> putStr "No google client key, some google features may not work" >> return ""
    allowForcedLogins <- doesFileExist "config/common/allows_forced_login"
    let checkToken t = do
          let x = readSignedWithKey @(Id Account) csk t
          pure x
    -- forkIO (addLoad csk)
    withDb "db" $ \pool -> do
      catch (withResource pool $ \conn -> withTransactionSerializable conn $
        runMigrations conn) $ \(e :: SomeException) -> (putStr "Waiting for DB fixes\n" >> threadDelay 100000000)
      let queryHandlerFn = \q -> fromMaybe emptyV <$> mapDecomposedV (handleAuthMapQuery checkToken (privateQueryHandler pool)) q
      atomicWriteIORef Common.View.globalMagicQueryHandler $ Just (fmap (head . Data.Map.Monoidal.elems . disperseV) . queryHandlerFn . condenseV . Data.Map.Monoidal.singleton (ClientKey 0))
      (listen, _) <- liftIO $ serveDbOverWebsockets
        (coerce pool)
        (requestHandler pool csk cgk allowForcedLogins)
        (\nm q -> fmap (fromMaybe emptyV) $ mapDecomposedV (handleAuthMapQuery checkToken (notifyHandler pool nm)) q)
        (QueryHandler $ queryHandlerFn)-- fromMaybe emptyV <$> mapDecomposedV (handleAuthMapQuery checkToken (privateQueryHandler pool)) q)
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

