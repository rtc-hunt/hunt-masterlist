{-# LANGUAGE ScopedTypeVariables #-}
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
import Rhyolite.Backend.Account
import Rhyolite.Backend.App
import Rhyolite.Backend.DB
import qualified Web.ClientSession as CS

import Prelude hiding (id, (.))
import Backend.Listen
import Backend.View
import Common.View
import Control.Category
import Data.Functor.Const
import Data.Functor.Sum
import Data.Constraint.Extras
import Data.Map.Monoidal (MonoidalMap)
import qualified Data.Map.Monoidal as MMap
import Reflex.Query.Class (SelectedCount(..))
import Data.GADT.Compare

simpleAuthPipeline
  :: forall m err auth k. (Has View k, GCompare k, Ord auth, Monad m)
  => FilterV m err auth k
  -> Pipeline m
       (MonoidalMap auth (Vessel k (Const SelectedCount)))
       (Vessel k (Compose (WithAuth err auth) (Const SelectedCount)))
simpleAuthPipeline fv = Pipeline $ \qh r -> pure $ (authQueryHandler qh, authRecipient r)
 where
  -- TODO: Use a newtype to give a different QueryResult instance for an auth map so that
  -- the errors can be returned too.
  sumF f g = \case
    InL x -> f x
    InR x -> g x
  disperseErrors = disperseV
                 . mapV (Compose . MMap.mapMaybe (sumF (\_ -> Nothing) Just) . getCompose)
  authQueryHandler
    :: QueryHandler (Vessel k (Compose (WithAuth err auth) (Const SelectedCount))) m
    -> QueryHandler (MonoidalMap auth (Vessel k (Const SelectedCount))) m
  authQueryHandler qh = QueryHandler $ \qs -> do
    r' <- runQueryHandler qh (condenseWithAuth qs)
    r <- buildV r' (unFilterV fv)
    pure $ disperseErrors r
  authRecipient
    :: Recipient (MonoidalMap auth (Vessel k (Const SelectedCount))) m
    -> Recipient (Vessel k (Compose (WithAuth err auth) (Const SelectedCount))) m
  authRecipient r = Recipient $ \qr' -> do
    qr <- buildV qr' (unFilterV fv)
    tellRecipient r $ disperseErrors qr

{-
backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
    csk <- CS.getKey "config/backend/clientSessionKey"
    withDb "db" $ \db -> do
      runNoLoggingT $ runDb (Identity db) $ do
        tables <- getTableAnalysis
        runMigration $ do
          migrateAccount tables
          migrateSchema tables
      (listen, _) <- liftIO $ serveDbOverWebsockets
        (coerce db)
        (requestHandler db csk)
        undefined -- (\nm q -> fmap (fromMaybe emptyV) $ mapDecomposedV (notifyHandler db nm) q)
        undefined -- (QueryHandler $ \q -> fromMaybe emptyV <$> mapDecomposedV (privateQueryHandler db) q)
        vesselFromWire
        vesselPipeline
      serve $ \case
        BackendRoute_Listen :/ () -> listen
        BackendRoute_Missing :/ () -> return ()
  , _backend_routeEncoder = fullRouteEncoder
  }
-}
