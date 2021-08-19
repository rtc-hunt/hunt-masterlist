module Backend.Request where

import Control.Monad.IO.Class
import Data.Functor.Identity
import Database.Groundhog
import Rhyolite.Account
import Rhyolite.Backend.Account (login)
import Rhyolite.Backend.App
import Rhyolite.Backend.DB (Db, getTime)
import Rhyolite.Backend.Sign
import Rhyolite.Sign
import Web.ClientSession as CS

import Backend.Schema
import Common.Api
import Common.Schema

requestHandler :: (MonadIO m', Db m') => (forall a. m' a -> IO a) -> CS.Key -> RequestHandler (Request (Signed (AuthToken Identity))) IO
requestHandler runDb csk = RequestHandler $ \case
  Request_Private token (PrivateRequest_SendMessage room content) -> do
    case readSignedWithKey csk token of
      Just (AuthToken (Identity user)) -> runDb $ do
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
  Request_Public (PublicRequest_Login user pass) -> runDb $ login (signWithKey csk) user pass >>= pure . \case
    Nothing -> Left "Those credentials didn't work"
    Just a -> Right a
