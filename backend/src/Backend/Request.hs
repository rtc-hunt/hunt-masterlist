{-# OPTIONS_GHC -Werror=missing-fields -Werror=incomplete-patterns #-}
module Backend.Request where

import Control.Monad.Except
import Control.Lens
import Crypto.JOSE.JWK (JWKSet)
import qualified Crypto.JWT as JWT
import Data.Aeson as Aeson
import Data.Aeson.Lens (_String)
import Data.ByteString.Lazy (fromStrict)
import Data.Maybe (fromMaybe)
import Data.Text.Encoding
import Data.Pool
import Data.Signed.ClientSession
import Data.Text (Text)
import Database.Beam
import Database.Beam.Postgres
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Class
import System.IO (stderr)
-- import Rhyolite.Account
import Rhyolite.Api
-- import Rhyolite.Backend.Account
import Rhyolite.Backend.App
import Rhyolite.DB.NotifyListen.Beam
import Web.ClientSession as CS
import Network.Google.Drive (driveFileScope, filesCreate, file, fMimeType, fName, fParents, fId)
import qualified Network.Google as Google
import System.Directory
import System.Environment

import Backend.Db (runDb, current_timestamp_)
import Backend.Listen ()
import Backend.Schema
-- import Common.Auth
import Common.Request
import Common.Schema

import Debug.Trace

createSheet :: Text -> IO (Maybe Text)
createSheet name = do
    canonicalizePath "config/backend" >>= setEnv "CLOUDSDK_CONFIG"
    lgr  <- Google.newLogger Google.Debug stderr
    env  <- Google.newEnv <&> (Google.envLogger .~ lgr) . (Google.envScopes .~ driveFileScope) -- (2) (3)
    qqq  <- Google.runResourceT . Google.runGoogle env $
      Google.send $ filesCreate $ file & (fMimeType ?~ "application/vnd.google-apps.spreadsheet") . (fName ?~ name) . (fParents .~ ["1F40xJAGFXFE8Z64xw_gxSR_P8TBYrwsi"])
    return $ qqq ^. fId

requestHandler
  :: Pool Connection
  -> CS.Key
  -> JWKSet
  -> Text
  -> RequestHandler (ApiRequest AuthToken PublicRequest PrivateRequest) IO
requestHandler pool csk gsk authAudience = RequestHandler $ \case
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
        let sheet = Nothing
        sheet <- liftIO $ do
          createSheet title
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
          , _puzzle_Channel = val_ $ (ChatroomId . fmap unChatroomId) $ channelId
          , _puzzle_Hunt = val_ $ hunt
          , _puzzle_removed = val_ $ Nothing
          , _puzzle_voicelink = val_ $ Nothing
          }
        pure $ case puzzle of
          Nothing -> Left "Couldn't create channel"
          Just cid -> Right cid
      PrivateRequest_UpdatePuzzle pzl -> auth $ \_user -> runDb pool $ do
        updateAndNotifyChange (_db_puzzles db) (primaryKey pzl) $ (<-. val_ pzl)
        return $ Right ()
      PrivateRequest_Renick newNick -> auth $ \user -> runDb pool $ do
        updateAndNotify (_db_account db) user 
          (\u -> _account_name u <-. val_ newNick)
        pure $ Right ()
      PrivateRequest_PuzzleCommand (PuzzleCommand_Tag pzl tag) -> auth $ \_user -> runDb pool $ do
        insertAndNotifyChange (_db_tags db) $ Tag (val_ pzl) (val_ tag)
        pure $ Right ()
      PrivateRequest_PuzzleCommand (PuzzleCommand_Untag pzl tag) -> auth $ \_user -> runDb pool $ do
        deleteAndNotifyChange (_db_tags db) $ TagId pzl tag
        pure $ Right ()
      PrivateRequest_PuzzleCommand (PuzzleCommand_Note pzl note) -> auth $ \_user -> runDb pool $ do
        insertAndNotifyChange (_db_notes db) $ Note default_ (val_ pzl) (val_ note)
        pure $ Right ()
      PrivateRequest_PuzzleCommand (PuzzleCommand_Solve pzl solution isBack) -> auth $ \_user -> runDb pool $ do
        insertAndNotifyChange (_db_solves db) $ Solution (val_ pzl) (val_ solution) (val_ isBack)
        pure $ Right ()
      PrivateRequest_PuzzleCommand (PuzzleCommand_AddMeta pzl meta) -> auth $ \_user -> runDb pool $ do
        insertAndNotifyChange (_db_metas db) $ Metapuzzle (val_ pzl) (val_ meta)
        pure $ Right ()
      PrivateRequest_PuzzleCommand (PuzzleCommand_UnSolve slv) -> auth $ \_user -> runDb pool $ do
        deleteAndNotifyChange (_db_solves db) $ slv
        pure $ Right ()
      PrivateRequest_PuzzleCommand (PuzzleCommand_RemoveMeta meta) -> auth $ \_user -> runDb pool $ do
        deleteAndNotifyChange (_db_metas db) $ meta
        pure $ Right ()
      PrivateRequest_PuzzleCommand (PuzzleCommand_DeletePuzzle puzId) -> auth $ \_user -> runDb pool $ do
        updateAndNotifyChange (_db_puzzles db) puzId $ (\p -> _puzzle_removed p <-. val_ (Just True))
        pure $ Right ()
      PrivateRequest_PuzzleCommand (PuzzleCommand_Voice puzId chatUrl) -> auth $ \_user -> runDb pool $ do
        updateAndNotifyChange (_db_puzzles db) puzId $ (\p -> _puzzle_voicelink p <-. val_ chatUrl)
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
  ApiRequest_Public (PublicRequest_GoogleLogin token) -> do
      traceM ("TOKEN: " <> show token)
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
              traceM $ show $ (subject, name)
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
          -- traceM "Bad Token"
          return $ Left "Bad token"
        Right claims -> handleValidClaims claims
