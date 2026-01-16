{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
module Backend.Eval where

import Language.Haskell.Interpreter hiding (eval)
import Language.Haskell.Interpreter.Unsafe
-- import Language.Haskell.Interpreter.Unsafe

import Common.Schema
import GHC.Generics
import Backend.Schema
import qualified Data.Text as T
import Data.Text (Text)
import Data.Yaml
import Data.Maybe (fromMaybe)
import Data.Int
import Data.Pool
import Data.Dependent.Sum (DSum(..))
import Data.Functor.Identity
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Beam ()
import Database.Beam hiding (set)
import Database.Beam.Backend.SQL.Types
import Database.Beam.Postgres
import Gargoyle.PostgreSQL.Connect
import Backend.Db (runDb, current_timestamp_)
import Rhyolite.DB.NotifyListen.Beam
import Rhyolite.DB.NotifyListen
import Backend.Schema
import Backend.Listen (Notify(..), CLIOutput(..))
import System.Process
import System.Environment
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad
import Data.List

-- pkgs = [("Prelude", Nothing), ("Gogol", Nothing), ("Crossword", Nothing)]

data EvalConfig = EvalConfig
  { ghcLibraries :: String
  , path :: Maybe String
  , imports :: [String]
  , qimports :: Maybe [(String, Maybe String)]
  } deriving (Generic, Show)

instance FromJSON EvalConfig
instance ToJSON EvalConfig

withInterpreter m = do
    EvalConfig { ghcLibraries, imports, qimports } <- decodeFileThrow "config/backend/tools"
    let finalImports = (fromMaybe [] qimports) <> (flip (,) Nothing <$> imports)
    unsafeRunInterpreterWithArgsLibdir [] ghcLibraries $ do
      setImportsQ finalImports
      set $ [ languageExtensions := [Safe] ]
      m

eval :: Text -> IO (Either InterpreterError Text)
eval expT = withInterpreter $ do
        rv <- interpret ("take 4096 $ show " ++ parens exp) (as :: String) 
        pure $ T.pack rv 
  where
    exp = T.unpack expT


evalExternally :: (Pool Connection) -> Int64 -> IO ()
evalExternally pool theId = runProcessInInterpreterEnv myProc fail
  where
    myProc = proc "worker" [show theId]
    fail = void $ runDb pool $ updateAndNotify (_db_evalJobs db) (EvalJobId $ SqlSerial theId) $ (\u -> _evalJob_error u <-. val_ (Just "Evaluator exceeded maximum run time (1 minute)"))
{-# 
evalExternally pool theId = void $ forkIO $ do
  EvalConfig { ghcLibraries } <- decodeFileThrow "config/backend/tools"
  myEnv <- getEnvironment
  let updatedEnv = [("NIX_GHC_LIBDIR", ghcLibraries)] <> filter ((/= "NIX_GHC_LIBDIR") . fst) myEnv 
  (_, _, _, ph) <- createProcess $ (proc "worker" [show theId]) {
    env = Just updatedEnv
  }
  threadDelay 60000000
  ec <- getProcessExitCode ph
  case ec of
    Just _ -> pure ()
    Nothing -> do
      terminateProcess ph
      void $ runDb pool $ updateAndNotify (_db_evalJobs db) (EvalJobId $ SqlSerial theId) $ (\u -> _evalJob_error u <-. val_ (Just "Evaluator exceeded maximum run time (1 minute)"))
#-}

runProcessInInterpreterEnv :: CreateProcess -> IO () -> IO ()
runProcessInInterpreterEnv myProc myFail = void $ forkIO $ do
  EvalConfig { ghcLibraries , path } <- decodeFileThrow "config/backend/tools"
  myEnv <- getEnvironment
  pathEnv <- getEnv "PATH"
  let newPath = fromMaybe "" $ fmap (<> ":") path
  putStrLn $ show newPath
  let updatedEnv = [("PATH", newPath <> pathEnv), ("NIX_GHC_LIBDIR", ghcLibraries)] <> filter ((/= "NIX_GHC_LIBDIR") . fst) myEnv 
  let newproc = case cmdspec myProc of
        RawCommand fp args -> myProc { cmdspec = RawCommand (fromMaybe fp ((<> fp) <$> path)) args }
        _ -> myProc
  putStrLn $ show ("Running worker", newproc)
  let newRawCommand = cmdspec myProc
  (_, _, _, ph) <- createProcess $ newproc {
    env = Just updatedEnv
  }
  threadDelay 60000000
  ec <- getProcessExitCode ph
  case ec of
    Just _ -> pure ()
    Nothing -> do
      terminateProcess ph
      myFail

muevalExternally :: String -> IO ()
muevalExternally theExpr = runProcessInInterpreterEnv myProc fail
  where
    myProc = proc "muworker" [theExpr]
    fail = putStrLn "Worker terminated for exceeding runtime"

doEval :: Int64 -> IO ()
doEval id = withDb "db" $ \pool ->
  runDb pool $ do
    Just cmd <- runSelectReturningOne $ lookup_ (_db_evalJobs db) $ EvalJobId $ SqlSerial id
    res <- liftIO $ eval $ _evalJob_expression cmd
    case res of
      Right res -> updateAndNotify (_db_evalJobs db) (primaryKey cmd) $ (\u -> _evalJob_result u <-. val_ (Just res))
      Left err -> updateAndNotify (_db_evalJobs db) (primaryKey cmd) $ (\u -> _evalJob_error u <-. val_ (Just $ T.pack $ show err))
    pure ()

muworkerMain :: IO ()
muworkerMain = do
  toEval <- fmap T.pack <$> getArgs
  forM_ toEval $ \expr -> do
    eval expr >>= putStrLn . show

workerMain :: IO ()
workerMain = do
  evalJobIds <- fmap read <$> getArgs
  fmap mconcat $ sequence $ doEval <$> evalJobIds

loopWorkerMain :: IO ()
loopWorkerMain = withDb "db" $ \pool -> do
  (notifs, done) <- startNotificationListener pool
  forever $ do
    notif <- notifs
    case notif of
      (DbNotification
        { _dbNotification_notificationType = NotificationType_Insert
        , _dbNotification_message = (Notify_EvalJob :=> Identity jobId) -- (na :: DSum Notify Identity)
        }) -> do
             evalExternally pool $ unSerial $ _evalJobId_id jobId
             pure ()
      _ -> pure ()
