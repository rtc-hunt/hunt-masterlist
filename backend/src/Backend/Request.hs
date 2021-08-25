module Backend.Request where

import Control.Monad.Logger
import Data.Functor.Identity
import Database.Groundhog
import Database.PostgreSQL.Simple
import Data.Pool
import Rhyolite.Backend.Account (login)
import Rhyolite.Backend.App
import Rhyolite.Backend.DB
import Rhyolite.Backend.Sign
import Rhyolite.Sign
import Web.ClientSession as CS

import Backend.Schema ()
import Common.Api
import Common.Schema

requestHandler
  :: Pool Connection
  -> CS.Key
  -> RequestHandler (Request (Signed (AuthToken Identity))) IO
requestHandler db csk = RequestHandler $ \case
  Request_Private token (PrivateRequest_SendMessage room content) -> do
    case readSignedWithKey csk token of
      Just (AuthToken (Identity user)) -> runNoLoggingT $ runDb (Identity db) $ do
        t <- getTime
        let msg = Message
              { _message_chatroom = room
              , _message_text = content
              , _message_timestamp = t
              , _message_account = user
              }
        _ <- insert_ msg
        pure $ Right ()
      _ -> pure $ Left "Unauthorized"
  Request_Public (PublicRequest_Login user pass) -> do
    loginResult <- runNoLoggingT $ runDb (Identity db) $ login pure user pass
    case loginResult of
      Nothing -> pure $ Left "Those credentials didn't work"
      Just a -> Right <$> signWithKey csk a
