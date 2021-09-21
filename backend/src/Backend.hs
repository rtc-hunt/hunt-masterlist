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
import Data.List (foldl')
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
       (AuthMap err auth (Vessel k) (Const SelectedCount))
       (Vessel k (Compose (WithAuth err auth) (Const SelectedCount)))
simpleAuthPipeline fv = Pipeline $ \qh r -> pure $ (authQueryHandler qh, authRecipient r)
 where
  partitionErrors :: Vessel k (Sum err Identity) -> (Vessel k err, Vessel k Identity)
  partitionErrors r = (fromListV errVs, fromListV valVs)
   where
    (errVs, valVs) = foldr go (mempty, mempty) (toListV r)
    go (k :~> v) (errs, vals) = has @View k $ case mapMaybeV getL v of
      Nothing -> case mapMaybeV getR v of
        Nothing -> (errs, vals)
        Just val -> (errs, (k:~>val):vals)
      Just err -> ((k:~>err):errs, vals)
    getL = \case
      InL x -> Just x
      InR _ -> Nothing
    getR = \case
      InL _ -> Nothing
      InR x -> Just x
  authQueryHandler
    :: QueryHandler (Vessel k (Compose (WithAuth err auth) (Const SelectedCount))) m
    -> QueryHandler (AuthMap err auth (Vessel k) (Const SelectedCount)) m
  authQueryHandler qh = QueryHandler $ \qs -> do
    r' <- runQueryHandler qh (condenseAuthMap qs)
    r <- buildV r' (unFilterV fv)
    pure $ fmap partitionErrors (disperseV r)
  authRecipient
    :: Recipient (AuthMap err auth (Vessel k) (Const SelectedCount)) m
    -> Recipient (Vessel k (Compose (WithAuth err auth) (Const SelectedCount))) m
  authRecipient r = Recipient $ \qr' -> do
    qr <- buildV qr' (unFilterV fv)
    tellRecipient r $ fmap partitionErrors (disperseV qr)

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
