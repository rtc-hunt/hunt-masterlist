{-# OPTIONS_GHC -Werror=missing-fields -Werror=incomplete-patterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Backend.Google where

import Gogol.Drive
import Gogol
import Data.Text (Text)
import Data.Proxy
import Control.Exception (SomeException, handle)
import System.Environment
import System.Directory
import System.IO (stderr)
import Control.Lens
import Data.Maybe


createPuzzleFiles :: Maybe Text -> Text -> IO (Maybe Text, Maybe Text)
createPuzzleFiles huntRootFolderId newName = handle failedCreateSheet $ do
    canonicalizePath "config/backend" >>= setEnv "CLOUDSDK_CONFIG" 
    lgr  <- newLogger Debug stderr
    env <- newEnv <&> (envLogger .~ lgr) . (envScopes .~ (Proxy :: Proxy '[Drive'File]))
    fldr <- runResourceT $ 
      send env $ newDriveFilesCreate $ newFile { mimeType = Just "application/vnd.google-apps.folder", name = Just newName, parents = Just ([fromMaybe "1F40xJAGFXFE8Z64xw_gxSR_P8TBYrwsi" huntRootFolderId]) }
    let fldrId = fromJust $ (\File { id = i } -> i) fldr
    fil  <- runResourceT $
      send env $ newDriveFilesCreate $ newFile { mimeType = Just "application/vnd.google-apps.spreadsheet", name = Just newName, parents = Just [fldrId] }
    let lfileId = (\File { id = i } -> i) fil
    return $ (Just fldrId, lfileId)
  where failedCreateSheet (e :: SomeException) = putStrLn "Failed to create google sheet, please check configuration." >> pure (Nothing, Nothing)

createFolder :: Text -> IO (Maybe Text)
createFolder folderName = handle failedCreateSheet $ do
    canonicalizePath "config/backend" >>= setEnv "CLOUDSDK_CONFIG" 
    lgr  <- newLogger Debug stderr
    env <- newEnv <&> (envLogger .~ lgr) . (envScopes .~ (Proxy :: Proxy '[Drive'File]))
    fldr <- runResourceT $ 
      send env $ newDriveFilesCreate $ newFile { mimeType = Just "application/vnd.google-apps.folder", name = Just folderName, parents = Just ["1F40xJAGFXFE8Z64xw_gxSR_P8TBYrwsi"] }
    let fldrId = (\File { id = i } -> i) fldr
    return $ fldrId
  where failedCreateSheet (e :: SomeException) = putStrLn "Failed to create google sheet, please check configuration." >> pure Nothing
