module Backend.Request where

import Control.Monad.Logger
import Data.Functor.Identity
import Database.Groundhog
import Database.Id.Groundhog
import Database.PostgreSQL.Simple
import Data.Pool
import Data.Text (Text)
import Rhyolite.Api
import Rhyolite.Backend.Account (login, ensureAccountExists, setAccountPassword)
import Rhyolite.Backend.App
import Rhyolite.Backend.DB
import Rhyolite.Backend.Sign
import Rhyolite.Sign
import Web.ClientSession as CS

import Backend.Listen
import Backend.Schema ()
import Common.Api
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
        _ <- insert_ msg
        pure $ Right ()
      PrivateRequest_CreateChatroom newName -> auth $ \_user -> runNoLoggingT $ runDb (Identity db) $ do
        eRes <- insertByAll $ Chatroom
          { _chatroom_title = newName
          }
        case eRes of
          Left _ -> pure $ Left "Could not create chatroom"
          Right cId -> pure $ Right (toId cId)
  ApiRequest_Public (PublicRequest_Login user pass) -> do
    loginResult <- runNoLoggingT $ runDb (Identity db) $ login pure user pass
    case loginResult of
      Nothing -> pure $ Left "Those credentials didn't work"
      Just a -> Right <$> signWithKey csk a
  ApiRequest_Public (PublicRequest_SignUp user pass) -> do
    res <- runNoLoggingT $ runDb (Identity db) $ do
      (new, aid) <- ensureAccountExists Notify_Account user
      case not new of
        True -> pure $ Left "Account already exists"
        False -> do
          setAccountPassword aid pass
          pure $ Right aid
    case res of
      Left err -> pure $ Left err
      Right aid ->
        Right <$> signWithKey csk aid
 where
