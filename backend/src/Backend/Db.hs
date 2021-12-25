module Backend.Db where

import Control.Monad.IO.Class
import Data.Pool
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Transaction
import Database.Beam.Postgres
import Database.Beam
import Data.Time
import Database.Beam.Postgres.Syntax

runDb :: MonadIO m => Pool Connection -> Pg a -> m a
runDb pool a = liftIO $ withResource pool $ \conn ->
  withTransactionSerializable conn $ runBeamPostgres conn a

runDbTrace :: MonadIO m => Pool Connection -> Pg a -> m a
runDbTrace pool a = liftIO $ withResource pool $ \conn ->
  withTransactionSerializable conn $ runBeamPostgresDebug putStrLn conn a

-- | Postgres @current_timestamp()@ function. Returns the server's timestamp
current_timestamp_ :: QExpr Postgres s UTCTime
current_timestamp_ = QExpr (\_ -> PgExpressionSyntax (emit "current_timestamp at time zone 'UTC'"))
