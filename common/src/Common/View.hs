{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
module Common.View where

import Data.Aeson
import Data.Aeson.GADT.TH
import Data.Constraint.Extras.TH
import Data.Maybe
import Data.Text
import Data.Time
import Data.Vessel
import Rhyolite.SemiMap
import Rhyolite.Sign
import Data.GADT.Compare.TH
import GHC.Generics
import Data.Semigroup

import Common.Schema

import Data.Constraint
import Data.Constraint.Extras
import Data.Foldable
import qualified Data.Functor.Sum as F
import Data.GADT.Compare
import Data.MonoidMap
import Data.Map.Monoidal (MonoidalMap)
import qualified Data.Map.Monoidal as MMap
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Void
import Data.Type.Equality
import Reflex.Query.Class

data MsgView = MsgView
  { _msgView_handle :: Text
  , _msgView_text :: Text
  }
  deriving (Generic, Eq)

instance ToJSON MsgView
instance FromJSON MsgView

data ChatroomQuery = ChatroomQuery
  { _chatroomQuery_search :: Text
  }
  deriving (Generic, Eq, Ord)

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

newtype WithAuth (err :: * -> *) auth a = WithAuth { unWithAuth :: (Set auth, a) }
  deriving (Functor)

instance (Ord auth, Semigroup a) => Semigroup (WithAuth err auth a) where
  WithAuth a <> WithAuth b = WithAuth $ a <> b

data AuthV auth public private (g :: (* -> *) -> *) where
  AuthV_Public :: AuthV auth public private public
  AuthV_Private :: auth -> AuthV auth public private private

deriveJSONGADT ''AuthV

instance Eq auth => GEq (AuthV auth public private) where
  geq = \case
    AuthV_Public -> \case
      AuthV_Public -> Just Refl
      _ -> Nothing
    AuthV_Private t0 -> \case
      AuthV_Private t2 -> case t0 == t2 of
        True -> Just Refl
        False -> Nothing
      _ -> Nothing

instance Ord auth => GCompare (AuthV auth public private) where
  gcompare = \case
    AuthV_Public -> \case
      AuthV_Public -> GEQ
      _ -> GLT
    AuthV_Private t0 -> \case
      AuthV_Public -> GGT
      AuthV_Private t1 -> case compare t0 t1 of
        LT -> GLT
        EQ -> GEQ
        GT -> GGT

instance ArgDict c (AuthV auth public private) where
  type ConstraintsFor (AuthV auth public private) c = (c public, c private)
  argDict = \case
    AuthV_Public -> Dict
    AuthV_Private _ -> Dict

decomposeAuthV
  :: (Ord auth, Semigroup (private g), Monoid (public g))
  => Vessel (AuthV auth public private) g
  -> (public g, MonoidalMap auth (private g))
decomposeAuthV authv = flip foldMap (toListV authv) $ \case
  AuthV_Public :~> pubv -> (pubv, mempty)
  AuthV_Private token :~> privv -> (mempty, MMap.singleton token privv)

condenseWithAuth
 :: ( View private
    , forall a. Monoid (g a)
    )
 => MonoidalMap auth (private g)
 -> private (Compose (WithAuth err auth) g)
condenseWithAuth = mapV (\(Compose m) -> Compose $ WithAuth (MMap.keysSet m, fold m)) . condenseV

mapDecomposedV'
  :: forall f g m v. (Functor f, Functor m, View v)
  => (v Proxy -> m (v Identity))
  -> v (Compose f g)
  -> m (Maybe (v (Compose f Identity)))
mapDecomposedV' f v = cropV recompose v <$> (f $ mapV (\_ -> Proxy) v)
  where
    recompose :: Compose f g a -> Identity a -> Compose f Identity a
    recompose (Compose s) i = Compose $ i <$ s

-- | Define a filter stage for a vessel-based view pipeline.
newtype FilterV m err auth k = FilterV
  { unFilterV
    :: forall v. k v
    -- ^ Which part of the view is being filtered?
    -> v (Compose (WithAuth err auth) Identity)
    -- ^ What is the main result and who are the recipients?
    -> m (v (Compose (MonoidalMap auth) (F.Sum err Identity)))
  }

-- | A filter that never throws an error and returns the full result to all recipients.
passthroughFilterV :: (Has View k, GCompare k, Applicative m) => FilterV m (Const Void) auth k
passthroughFilterV = FilterV $ \k -> has @View k $ (pure .) $ mapV $ \(Compose (WithAuth (auths, Identity m))) -> Compose $
  MMap.fromSet (\_ -> F.InR (Identity m)) auths

{-
authVesselFromWire
  :: ()
  => QueryMorphism (AuthV auth public private (Const ())) (AuthV auth public private (Const SelectedCount))
authVesselFromWire = QueryMorphism
  { _queryMorphism_mapQuery = mapV (mapV (const (Const 1)))
  , _queryMorphism_mapQueryResult = id
  }
-}
{-
-- | Reverses vesselToWire
vesselFromWire
  :: ( View v
     , QueryResult (v (Const ())) ~ v Identity
     , QueryResult (v (Const SelectedCount)) ~ v Identity
     )
  => QueryMorphism (v (Const ())) (v (Const SelectedCount))
vesselFromWire = QueryMorphism
  { _queryMorphism_mapQuery = mapV (const (Const 1))
  , _queryMorphism_mapQueryResult = id
  }
-}

data EmptyV (g :: (* -> *))

type PublicChatV = EmptyV
type PrivateChatV = Vessel V

type AuthChatV a = Vessel (AuthV (Signed (AuthToken Identity)) PublicChatV PrivateChatV) a

