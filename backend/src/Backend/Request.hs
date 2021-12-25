module Backend.Request where

import Data.Pool
import Data.Signed.ClientSession
import Data.Text (Text)
import Database.Beam
import Database.Beam.Postgres
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Class
import Rhyolite.Account
import Rhyolite.Api
import Rhyolite.Backend.Account
import Rhyolite.Backend.App
import Rhyolite.DB.NotifyListen.Beam
import Web.ClientSession as CS

import Backend.Db (runDb, current_timestamp_)
import Backend.Listen ()
import Backend.Schema
import Common.Request
import Common.Schema

requestHandler
  :: Pool Connection
  -> CS.Key
  -> RequestHandler (ApiRequest AuthToken PublicRequest PrivateRequest) IO
requestHandler pool csk = RequestHandler $ \case
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

  ApiRequest_Public (PublicRequest_Login user pass) -> do
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
        pure (Right x)
