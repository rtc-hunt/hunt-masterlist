{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
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

data MsgView = MsgView
  { _msgView_id :: Id Message
  , _msgView_timestamp :: UTCTime
  , _msgView_handle :: Text
  , _msgView_text :: Text
  }
  deriving (Show, Generic, Eq)

instance ToJSON MsgView
instance FromJSON MsgView

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
  V_Messages :: V (SubVessel (Id Chatroom) (MapV RequestInterval (SemiMap Int MsgView)))

deriving instance Show (V a)

deriveArgDict ''V
deriveJSONGADT ''V
deriveGEq ''V
deriveGShow ''V
deriveGCompare ''V

type PrivateChatV = Vessel V
