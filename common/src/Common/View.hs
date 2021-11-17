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
import Data.GADT.Compare.TH
import GHC.Generics
import Data.Semigroup

import Common.Schema

data MsgView = MsgView
  { _msgView_handle :: Text
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

data V a where
  V_Chatrooms :: V (MapV ChatroomQuery (SemiMap (Id Chatroom) Text))
  V_Chatroom :: V (MapV (Id Chatroom) (First Text))
  V_Messages :: V (MapV (Id Chatroom) (SemiMap UTCTime [MsgView]))

deriveArgDict ''V
deriveJSONGADT ''V
deriveGEq ''V
deriveGCompare ''V

type PrivateChatV = Vessel V
