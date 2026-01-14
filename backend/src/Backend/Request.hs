{-# OPTIONS_GHC -Werror=missing-fields -Werror=incomplete-patterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
module Backend.Request where

import Control.Monad.Except
import Control.Lens
import Crypto.JOSE.JWK (JWKSet)
import qualified Crypto.JWT as JWT
import Data.Aeson.Lens (_String)
import Data.ByteString.Lazy (fromStrict)
import qualified Data.ByteString.Char8 as C8
import Data.Monoid (First(..))
import Text.Read
import Data.Time.Clock
import Data.Maybe (fromMaybe)
import Data.Text.Encoding
import Data.Pool
import Data.Signed (Signed)
import Data.Signed.ClientSession
import Data.Text (Text)
import Data.Dependent.Sum
import Database.Beam
import Database.Beam.Postgres
import Database.Beam.Postgres.Full hiding (insert)
import qualified Database.Beam.Postgres.Full as Pg
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Class
import System.IO (stderr)
import Rhyolite.Api
import Rhyolite.Backend.App
import Rhyolite.DB.NotifyListen.Beam
import Web.ClientSession as CS
import Data.Proxy
import Network.HTTP.Req hiding (req)
import qualified Network.HTTP.Req as Req
import System.Directory
import System.Environment
import Control.Exception (SomeException, handle)

import Backend.Db (runDb, current_timestamp_)
import Backend.Listen ()
import Backend.Schema
-- import Common.Auth
import Common.Request
import Common.Schema
import Backend.Google

import Debug.Trace

type RequestHandlerType = RequestHandler (ApiRequest AuthToken PublicRequest PrivateRequest) IO

requestHandler
  :: Pool Connection
  -> CS.Key
  -> Text
  -> Bool
  -> RequestHandler (ApiRequest AuthToken PublicRequest PrivateRequest) IO
requestHandler pool csk authAudience allowForcedLogins = RequestHandler $ \case
  ApiRequest_Private token req -> do
    let auth
          :: Applicative m
          => (Id Account -> m (Either Text a))
          -> m (Either Text a)
        auth k = case readSignedWithKey csk token of
          Just user -> k user
          Nothing -> pure $ Left "Unauthorized"
    case req of
      PrivateRequest_SendMessage room content -> auth $ \user -> runDb pool $ do
        _ <- insertAndNotify (_db_message db) $ Message
          { _message_id = default_
          , _message_chatroom = val_ room
          , _message_text = val_ content
          , _message_timestamp = current_timestamp_
          , _message_account = val_ user
          , _message_isMe = val_ (Just False)
          }
        pure $ Right ()
      PrivateRequest_SendMe room content -> auth $ \user -> runDb pool $ do
        _ <- insertAndNotify (_db_message db) $ Message
          { _message_id = default_
          , _message_chatroom = val_ room
          , _message_text = val_ content
          , _message_timestamp = current_timestamp_
          , _message_account = val_ user
          , _message_isMe = val_ (Just True)
          }
        pure $ Right ()
      PrivateRequest_CreateChatroom newName -> auth $ \_user -> runDb pool $ do
        mcid <- insertAndNotify (_db_chatroom db) Chatroom
          { _chatroom_id = default_
          , _chatroom_title = val_ newName
          }
        pure $ case mcid of
          Nothing -> Left "Couldn't create channel"
          Just cid -> Right cid
      PrivateRequest_LatestMessage cid -> auth $ \_ -> runDb pool $ do
        res <- [iquery| select count(*) from db_message m where m.message_chatroom__chatroom_id = ${cid} |]
        case res of
          [Only n] -> pure . Right $ (cid, n)
          _ -> pure . Left $ "PrivateRequest_LatestMessage: got more than one row"
      PrivateRequest_AddPuzzle title ismeta url hunt -> auth $ \_user -> runDb pool $ do
        huntRecord <- runSelectReturningOne $ lookup_ (_db_hunt db) hunt
        (folderId, sheet) <- liftIO $ do
          createPuzzleFiles (huntRecord >>= _hunt_folderId) title
        -- Also queue create new puzzle spreadsheet here through job system.
        channelId <-  insertAndNotify (_db_chatroom db) Chatroom
          { _chatroom_id = default_
          , _chatroom_title = val_ title
          }
        puzzle <- insertAndNotifyChange (_db_puzzles db) Puzzle
          { _puzzle_id = default_
          , _puzzle_Title = val_ title
          , _puzzle_URI = val_ url
          , _puzzle_IsMeta = val_ ismeta
          , _puzzle_StartedAt = val_ Nothing
          , _puzzle_SheetURI = val_ $ (\a->"https://docs.google.com/spreadsheets/d/" <> a <> "/edit") <$> sheet
          , _puzzle_FolderId = val_ $ folderId
          , _puzzle_Channel = val_ $ (ChatroomId . fmap unChatroomId) $ channelId
          , _puzzle_Hunt = val_ $ hunt
          , _puzzle_removed = val_ $ Nothing
          , _puzzle_voicelink = val_ $ Nothing
          }
        pure $ case puzzle of
          Nothing -> Left "Couldn't create channel"
          Just cid -> Right cid
      PrivateRequest_UpdatePuzzle pzl -> auth $ \_user -> runDb pool $ do
        _ <- updateAndNotifyChange (_db_puzzles db) (primaryKey pzl) $ (<-. val_ pzl)
        return $ Right ()
      PrivateRequest_Renick newNick -> auth $ \user -> runDb pool $ do
        _ <- updateAndNotify (_db_account db) user 
          (\u -> _account_name u <-. val_ newNick)
        pure $ Right ()
      PrivateRequest_NewHunt hunt_title hunt_rootpage -> auth $ \_user -> runDb pool $ do
        channelId <-  insertAndNotify (_db_chatroom db) Chatroom
          { _chatroom_id = default_
          , _chatroom_title = val_ $ hunt_title
          }
        huntFolder <- liftIO $ createFolder hunt_title
        case channelId of
          Nothing -> return $ Left "Couldn't create channel for hunt"
          Just channelId -> do
            huntId <- insertAndNotify (_db_hunt db) Hunt
              { _hunt_id = default_
              , _hunt_title = val_ $ hunt_title
              , _hunt_rootpage = val_ $ hunt_rootpage
              , _hunt_channel = val_ $ channelId
              , _hunt_live = val_ $ True
              , _hunt_folderId = val_ $ huntFolder
              }
            pure $ case huntId of
              Nothing -> Left "Couldn't create hunt"
              Just cid -> Right cid
      PrivateRequest_SaveSettings newSettings -> auth $ \user -> runDb pool $ do
        traceM "Saving settings"
        let toNotify = map $ \x -> notification (_db_userSettings db) :=> Identity x
        _ <- runPgInsertReturningListWithNotify toNotify $ flip Pg.returning primaryKey $ insertOnConflict (_db_userSettings db)
               (insertValues [UserSettingsTable (user) (newSettings)])
               (conflictingFields $ \tbl -> primaryKey tbl)
               (onConflictUpdateSet (\fields old -> fields <-. val_ (UserSettingsTable user newSettings)))
        pure $ Right ()
      PrivateRequest_PuzzleCommand (PuzzleCommand_Tag pzl tag) -> auth $ \_user -> runDb pool $ do
        _ <- insertAndNotifyChange (_db_tags db) $ Tag (val_ pzl) (val_ tag)
        pure $ Right ()
      PrivateRequest_PuzzleCommand (PuzzleCommand_Untag pzl tag) -> auth $ \_user -> runDb pool $ do
        _ <- deleteAndNotifyChange (_db_tags db) $ TagId pzl tag
        pure $ Right ()
      PrivateRequest_PuzzleCommand (PuzzleCommand_Note pzl note) -> auth $ \_user -> runDb pool $ do
        _ <- insertAndNotifyChange (_db_notes db) $ Note default_ (val_ pzl) (val_ note) (val_ True)
        pure $ Right ()
      PrivateRequest_PuzzleCommand (PuzzleCommand_SetNoteVisibility (note, vis)) -> auth $ \_user -> runDb pool $ do
        _ <- updateAndNotifyChange (_db_notes db) note $ (\n -> _note_active n <-. val_ vis)
        pure $ Right ()
      PrivateRequest_PuzzleCommand (PuzzleCommand_Solve pzl solution isBack) -> auth $ \_user -> runDb pool $ do
        _ <- insertAndNotifyChange (_db_solves db) $ Solution (val_ pzl) (val_ solution) (val_ isBack)
        pure $ Right ()
      PrivateRequest_PuzzleCommand (PuzzleCommand_AddMeta pzl meta) -> auth $ \_user -> runDb pool $ do
        _ <- insertAndNotifyChange (_db_metas db) $ Metapuzzle (val_ pzl) (val_ meta)
        pure $ Right ()
      PrivateRequest_PuzzleCommand (PuzzleCommand_UnSolve slv) -> auth $ \_user -> runDb pool $ do
        _ <- deleteAndNotifyChange (_db_solves db) $ slv
        pure $ Right ()
      PrivateRequest_PuzzleCommand (PuzzleCommand_RemoveMeta meta) -> auth $ \_user -> runDb pool $ do
        _ <- deleteAndNotifyChange (_db_metas db) $ meta
        pure $ Right ()
      PrivateRequest_PuzzleCommand (PuzzleCommand_DeletePuzzle puzId) -> auth $ \_user -> runDb pool $ do
        _ <- updateAndNotifyChange (_db_puzzles db) puzId $ (\p -> _puzzle_removed p <-. val_ (Just True))
        pure $ Right ()
      PrivateRequest_PuzzleCommand (PuzzleCommand_Voice puzId chatUrl) -> auth $ \_user -> runDb pool $ do
        _ <- updateAndNotifyChange (_db_puzzles db) puzId $ (\p -> _puzzle_voicelink p <-. val_ chatUrl)
        pure $ Right ()



  {-ApiRequest_Public (PublicRequest_Login user pass) -> do
    loginResult <- runDb pool $ login (_db_account db) user pass
    case loginResult of
      Nothing -> pure $ Left "Those credentials didn't work"
      Just a -> do
        x <- signWithKey csk a
        pure $ Right x
  ApiRequest_Public (PublicRequest_Signup user pass) -> do
    hash <- makePasswordHash pass
    res <- runDb pool $ do
      (new, aid) <- ensureAccountExists (_db_account db) user
      case not new of
        True -> pure $ Left "Account already exists"
        False -> do
          setAccountPasswordHash (_db_account db) aid hash
          pure $ Right aid
    case res of
      Left err -> pure $ Left err
      Right aid -> do
        x <- signWithKey csk aid
        pure (Right x) -}
  ApiRequest_Public (PublicRequest_GoogleLogin token) -> checkGoogleAuthToken pool authAudience csk token {-do
      let googuri = https "www.googleapis.com" /: "oauth2" /: "v3" /: "certs"
      lastGSK <- runDb pool $ runSelectReturningOne $ select $ limit_ 1 $ do
          k <- all_ (_db_googleKeys db)
          guard_ $ _googleKey_expires k Database.Beam.>. current_timestamp_
          pure $ _googleKey_keyset k
      gsk :: JWKSet <- case lastGSK of
           Just (PgJSON a) -> do
                return a
           Nothing -> do
                current <- getCurrentTime
                new <- runReq defaultHttpConfig $ Req.req GET googuri NoReqBody jsonResponse mempty
                let maxAge = fromMaybe 0 $ getFirst $ ((mconcat $ (First . (fmap (fromRational @NominalDiffTime ) .  readMaybe . C8.unpack <=< C8.stripSuffix "," <=< C8.stripPrefix "max-age=")) <$> (C8.words $ fromMaybe "" (responseHeader new "Cache-Control"))))
                    expires = addUTCTime maxAge current
                runDb pool $ runInsert $ insert (_db_googleKeys db) $ insertValues [GoogleKey
                  { _googleKey_fetchedAt = current
                  , _googleKey_expires = expires
                  , _googleKey_keyset = PgJSON $ responseBody new
                  }]
                return $ responseBody new
      (parsedToken :: Either JWT.JWTError JWT.ClaimsSet) <- runExceptT $ do
         let config = JWT.defaultJWTValidationSettings (const True . (== authAudience) . (^. JWT.string)) -- "570358826294-2ut7bnk6ar7jmqifsef48ljlk0o5m8p4.apps.googleusercontent.com"
         decoded <- JWT.decodeCompact $ fromStrict $ encodeUtf8 token
         JWT.verifyClaims config gsk decoded
      let handleValidClaims claims = do
          if not $ (claims ^. JWT.claimIss) `elem` [Just "https://accounts.google.com", Just "accounts.google.com"] 
            then return $ Left "Invalid claims"
            else do
              let subject = claims ^. JWT.claimSub . _Just . JWT.string
              let name = claims ^? JWT.unregisteredClaims . at "name" . _Just . _String
              -- let Just (Aeson.String email) = claims ^. JWT.unregisteredClaims . at "email"
              existingUser <- runDb pool $ runSelectReturningOne $ select $ do
                user <- all_ (_db_account db)
                guard_ $ _account_guid user ==. val_ subject
                return user
              case existingUser of
                Just someUser -> Right <$> signWithKey csk (AccountId $ _account_id someUser)
                Nothing -> do
                  muid <- runDb pool $ insertAndNotify (_db_account db) $ Account
                    { _account_id = default_
                    , _account_name = val_ $ fromMaybe "Unnamed" name
                    , _account_guid = val_ $ subject
                    }
                  case muid of
                    Nothing -> return $ Left "Couldn't create user"
                    Just uid -> Right <$> signWithKey csk (uid)
      case parsedToken of
        Left _ -> do
          return $ Left "Bad token"
        Right claims -> handleValidClaims claims
      -}
  ApiRequest_Public (PublicRequest_ForceLogin) -> do
      if not allowForcedLogins then return $ Left "Not allowed" else do
        existingUser <- runDb pool $ runSelectReturningOne $ select $ do
          user <- all_ (_db_account db)
          guard_ $ _account_name user ==. val_ "testuser"
          return user
        case existingUser of
          Just someUser -> Right <$> signWithKey csk (AccountId $ _account_id someUser)
          Nothing -> do
                  muid <- runDb pool $ insertAndNotify (_db_account db) $ Account
                    { _account_id = default_
                    , _account_name = val_ $ "testuser"
                    , _account_guid = val_ $ ""
                    }
                  case muid of
                    Nothing -> return $ Left "Couldn't create user"
                    Just uid -> Right <$> signWithKey csk (uid)


checkGoogleAuthToken :: (Pool Connection) -> Text -> Key -> Text -> IO (Either Text (Data.Signed.Signed (PrimaryKey Account Identity)))
checkGoogleAuthToken pool authAudience csk token = do
      traceM "Handling a login"
      let googuri = https "www.googleapis.com" /: "oauth2" /: "v3" /: "certs"
      lastGSK <- runDb pool $ runSelectReturningOne $ select $ limit_ 1 $ do
          k <- all_ (_db_googleKeys db)
          guard_ $ _googleKey_expires k Database.Beam.>. current_timestamp_
          pure $ _googleKey_keyset k
      gsk :: JWKSet <- case lastGSK of
           Just (PgJSON a) -> do
                return a
           Nothing -> do
                current <- getCurrentTime
                new <- runReq defaultHttpConfig $ Req.req GET googuri NoReqBody jsonResponse mempty
                let maxAge = fromMaybe 0 $ getFirst $ ((mconcat $ (First . (fmap (fromRational @NominalDiffTime ) .  readMaybe . C8.unpack <=< C8.stripSuffix "," <=< C8.stripPrefix "max-age=")) <$> (C8.words $ fromMaybe "" (responseHeader new "Cache-Control"))))
                    expires = addUTCTime maxAge current
                runDb pool $ runInsert $ insert (_db_googleKeys db) $ insertValues [GoogleKey
                  { _googleKey_fetchedAt = current
                  , _googleKey_expires = expires
                  , _googleKey_keyset = PgJSON $ responseBody new
                  }]
                return $ responseBody new
      (parsedToken :: Either JWT.JWTError JWT.ClaimsSet) <- runExceptT $ do
         let config = JWT.defaultJWTValidationSettings (const True . (== authAudience) . (^. JWT.string)) -- "570358826294-2ut7bnk6ar7jmqifsef48ljlk0o5m8p4.apps.googleusercontent.com"
         decoded <- JWT.decodeCompact $ fromStrict $ encodeUtf8 token
         JWT.verifyClaims config gsk decoded
      let handleValidClaims claims = do
          if not $ (claims ^. JWT.claimIss) `elem` [Just "https://accounts.google.com", Just "accounts.google.com"] 
            then return $ Left "Invalid claims"
            else do
              let subject = claims ^. JWT.claimSub . _Just . JWT.string
              let name = claims ^? JWT.unregisteredClaims . at "name" . _Just . _String
              -- let Just (Aeson.String email) = claims ^. JWT.unregisteredClaims . at "email"
              existingUser <- runDb pool $ runSelectReturningOne $ select $ do
                user <- all_ (_db_account db)
                guard_ $ _account_guid user ==. val_ subject
                return user
              case existingUser of
                Just someUser -> Right <$> signWithKey csk (AccountId $ _account_id someUser)
                Nothing -> do
                  muid <- runDb pool $ insertAndNotify (_db_account db) $ Account
                    { _account_id = default_
                    , _account_name = val_ $ fromMaybe "Unnamed" name
                    , _account_guid = val_ $ subject
                    }
                  case muid of
                    Nothing -> return $ Left "Couldn't create user"
                    Just uid -> Right <$> signWithKey csk (uid)
      case parsedToken of
        Left _ -> do
          return $ Left "Bad token"
        Right claims -> handleValidClaims claims
