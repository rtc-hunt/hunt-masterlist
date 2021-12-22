module Backend.Request where

import Control.Monad.Logger
import Data.Functor.Identity
import Database.PostgreSQL.Simple
import Data.Pool
import Data.Text (Text)
import Rhyolite.Api
import Rhyolite.Backend.App
import Database.PostgreSQL.Simple.Class
import Data.Signed
import Data.Signed.ClientSession
import Rhyolite.DB.Groundhog
import Rhyolite.DB.NotifyListen.Groundhog
import Rhyolite.Account.Groundhog
import Web.ClientSession as CS

import Backend.Listen
import Backend.Schema ()
import Common.Request
import Common.Schema

requestHandler
  :: Pool Connection
  -> CS.Key
  -> RequestHandler (ApiRequest (Signed (AuthToken Identity)) PublicRequest PrivateRequest) IO
requestHandler db csk = RequestHandler $ \case
  ApiRequest_Private token req -> do
    let auth
          :: Applicative m
          => (Id Account -> m (Either Text a))
          -> m (Either Text a)
        auth k = case readSignedWithKey csk token of
          Just (AuthToken (Identity user)) -> k user
          Nothing -> pure $ Left "Unauthorized"
    case req of
      PrivateRequest_SendMessage room content -> auth $ \user -> runNoLoggingT $ runDb (Identity db) $ do
        t <- getTime
        let msg = Message
              { _message_chatroom = room
              , _message_text = content
              , _message_timestamp = t
              , _message_account = user
              }
        _ <- insertAndNotify msg
        pure $ Right ()
      PrivateRequest_CreateChatroom newName -> auth $ \_user -> runNoLoggingT $ runDb (Identity db) $ do
        cId <- insertAndNotify $ Chatroom
          { _chatroom_title = newName
          }
        pure $ Right cId
      PrivateRequest_LatestMessage cid -> auth $ \_ -> runNoLoggingT $ runDb (Identity db) $ do
        res <- [iquery| select count(*) from "Message" m where m.chatroom = ${cid} |]
        case res of
          [Only n] -> pure . Right $ (cid, n)
          _ -> pure . Left $ "PrivateRequest_LatestMessage: got more than one row"

  ApiRequest_Public (PublicRequest_Login user pass) -> do
    loginResult <- runNoLoggingT $ runDb (Identity db) $ login pure user pass
    case loginResult of
      Nothing -> pure $ Left "Those credentials didn't work"
      Just a -> do
        x <- signWithKey csk (AuthToken (Identity a))
        pure $ Right x
  ApiRequest_Public (PublicRequest_Signup user pass) -> do
    res <- runNoLoggingT $ runDb (Identity db) $ do
      (new, aid) <- ensureAccountExists Notify_Account user
      case not new of
        True -> pure $ Left "Account already exists"
        False -> do
          setAccountPassword aid pass
          pure $ Right aid
    case res of
      Left err -> pure $ Left err
      Right aid -> do
        x <- signWithKey csk (AuthToken (Identity aid))
        pure (Right x)
