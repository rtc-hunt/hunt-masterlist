{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Common.View where

import Data.Aeson
import Data.Aeson.GADT.TH
import Data.Constraint.Extras.TH
import Data.Text
import Data.Time
import Data.Vessel
import Rhyolite.SemiMap
import Data.GADT.Show.TH
import Data.GADT.Compare.TH
import GHC.Generics hiding (from)
import Data.Semigroup

import Common.Schema

import Data.IORef
import Reflex.Query.Class (QueryResult, SelectedCount)
import System.IO.Unsafe
import Rhyolite.Api
import Common.Request
import Data.Vessel.Void
import Rhyolite.Vessel.App
import Rhyolite.Vessel.AuthenticatedV
import Rhyolite.Vessel.AuthMapV


data Msg = Msg
  { _msg_id :: Id Message
  , _msg_timestamp :: UTCTime
  , _msg_handle :: Text
  , _msg_text :: Text
  , _msg_isme :: Bool
  }
  deriving (Show, Generic, Eq)

instance ToJSON Msg
instance FromJSON Msg

data ChatroomQuery = ChatroomQuery
  { _chatroomQuery_search :: Text
  }
  deriving (Show, Generic, Eq, Ord)

instance ToJSON ChatroomQuery
instance FromJSON ChatroomQuery
instance ToJSONKey ChatroomQuery
instance FromJSONKey ChatroomQuery

data RequestInterval = RequestInterval { _requestInterval_point :: Int
                                       , _requestInterval_before :: Int
                                       , _requestInterval_after :: Int
                                       }
  deriving (Show, Read, Eq, Ord, Generic)

instance ToJSON RequestInterval
instance ToJSONKey RequestInterval
instance FromJSON RequestInterval
instance FromJSONKey RequestInterval

requestIntervalMax :: RequestInterval -> Int
requestIntervalMax ri = _requestInterval_point ri + _requestInterval_after ri

requestIntervalMin :: RequestInterval -> Int
requestIntervalMin ri = _requestInterval_point ri - _requestInterval_before ri

inRequestInterval :: RequestInterval -> Int -> Bool
inRequestInterval (RequestInterval point before after) n = point - before <= n && n <= point + after

data V a where
  V_Chatrooms :: V (MapV ChatroomQuery (SemiMap (Id Chatroom) Text))
  V_Chatroom :: V (MapV (Id Chatroom) (First Text))
  V_Messages :: V (SubVessel (Id Chatroom) (MapV RequestInterval (SemiMap Int Msg)))
  V_HuntPuzzles :: V (MapV (Id Hunt) (SemiSet (Id Puzzle)))
  V_Puzzle :: V (MapV (Id Puzzle) (First (Puzzle Identity)))
  V_Solutions :: V (MapV (Id Puzzle) (SemiMap (Id Solution) (Solution Identity)))
  V_Tags :: V (MapV (Id Puzzle) (SemiMap (Id Tag) (Tag Identity)))
  V_UniqueTags :: V (MapV () (SemiSet Text))
  V_Notes :: V (MapV (Id Puzzle) (SemiMap (Id Note) (Note Identity)))
  V_Metas :: V (MapV (Id Puzzle) (SemiSet (Metapuzzle Identity)))
  V_Hunts :: V (MapV () (SemiMap (Id Hunt) (Hunt Identity)))
  V_HuntMetas :: V (MapV (Id Hunt) (SemiMap (Id Puzzle) Text))
  V_ActiveUsers :: V (MapV (Id Chatroom) (SemiMap (Id Account) Text))
  V_LiveHunts :: V (MapV () (SemiSet (Id Hunt)))

deriving instance Show (V a)

deriveArgDict ''V
deriveJSONGADT ''V
deriveGEq ''V
deriveGShow ''V
deriveGCompare ''V

instance FromJSONKey ()
instance ToJSONKey ()

type PrivateChatV = Vessel V

data PersV a where
  PV_Settings :: PersV (SingleV (UserSettings Identity))
  PV_CLIOutput :: PersV (SingleV Text)

deriving instance Show (PersV a)
deriveArgDict ''PersV
deriveJSONGADT ''PersV
deriveGEq ''PersV
deriveGShow ''PersV
deriveGCompare ''PersV

type HMLPersonalV = Vessel PersV

type MyQueryType = FullAppV MasterlistApp (Const SelectedCount) -- AuthenticatedV VoidV (AuthMapV AuthToken PrivateChatV) VoidV (Const SelectedCount)
type MyQueryResultType = QueryResult MyQueryType -- AuthenticatedV VoidV (AuthMapV AuthToken PrivateChatV) VoidV (Const SelectedCount)
-- type MyQueryResultType = QueryResult (AuthMapV AuthToken PrivateChatV (Const SelectedCount))
-- type MagicQueryHandler m = ((AuthMapV AuthToken PrivateChatV (Const SelectedCount)) -> m (QueryResult (AuthMapV AuthToken PrivateChatV (Const SelectedCount))))

type MagicQueryHandler m = FullAppV MasterlistApp (Const SelectedCount) -> m (QueryResult (FullAppV MasterlistApp (Const SelectedCount)))

-- globalMagicQueryHandler :: IORef (Maybe (MagicQueryHandler IO))
-- globalMagicQueryHandler = unsafePerformIO $ newIORef Nothing

globalMagicQueryHandler :: IORef (Maybe (MagicQueryHandler IO))
globalMagicQueryHandler = unsafePerformIO $ newIORef Nothing

-- hotQueryHandler :: IORef (Maybe ((AuthMapV AuthToken PrivateChatV (Const SelectedCount)) -> IO (QueryResult (AuthMapV AuthToken PrivateChatV (Const SelectedCount)))))
-- hotQueryHandler = unsafePerformIO $ newIORef Nothing

data MasterlistApp

instance RhyoliteAuthApp MasterlistApp where
  type AuthCredential MasterlistApp = AuthToken
  type PublicApi MasterlistApp = PublicRequest
  type PrivateApi MasterlistApp = PrivateRequest
  type PrivateV MasterlistApp = PrivateChatV
  type PersonalV MasterlistApp = HMLPersonalV
  type PublicV MasterlistApp = VoidV

