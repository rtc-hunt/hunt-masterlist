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
import Data.Maybe
import Data.Text
import Data.Time
import Data.Vessel
import Data.Vessel.Single
import Data.Vessel.SubVessel
import Data.Vessel.Vessel
import Rhyolite.SemiMap
import Data.GADT.Compare.TH
import GHC.Generics
import Data.Semigroup

import Common.Schema

import Data.Constraint
import Data.Constraint.Extras
import Data.GADT.Compare
import Data.MonoidMap ()
import qualified Data.Map.Monoidal as MMap
import Data.Patch
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Type.Equality
import Data.Witherable
import Reflex.Query.Class
import Rhyolite.App

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

type PrivateChatV = Vessel V

newtype WithAuth (err :: * -> *) auth a = WithAuth { unWithAuth :: (Set auth, a) }
  deriving (Functor)

instance (Ord auth, Semigroup a) => Semigroup (WithAuth err auth a) where
  WithAuth a <> WithAuth b = WithAuth $ a <> b

data ErrorVK err view (v :: (* -> *) -> *) where
  ErrorVK_Error :: ErrorVK err view (SingleV err)
  ErrorVK_View :: ErrorVK err view view

deriveJSONGADT ''ErrorVK

instance GEq (ErrorVK err view) where
  geq = \case
    ErrorVK_Error -> \case
      ErrorVK_Error -> Just Refl
      ErrorVK_View -> Nothing
    ErrorVK_View -> \case
      ErrorVK_Error -> Nothing
      ErrorVK_View -> Just Refl

instance GCompare (ErrorVK err view) where
  gcompare = \case
    ErrorVK_Error -> \case
      ErrorVK_Error -> GEQ
      ErrorVK_View -> GLT
    ErrorVK_View -> \case
      ErrorVK_Error -> GGT
      ErrorVK_View -> GEQ

instance ArgDict c (ErrorVK err view) where
  type ConstraintsFor (ErrorVK err view) c = (c (SingleV err), c view)
  argDict = \case
    ErrorVK_Error -> Dict
    ErrorVK_View -> Dict

-- TODO: Abstract data type. Do not reexport
newtype ErrorV err view g = ErrorV { unErrorV :: Vessel (ErrorVK err view) g }
  deriving (Generic, EmptyView)

deriving instance (Eq (g (First (Maybe err))), Eq (view g)) => Eq (ErrorV err view g)

instance View view => View (ErrorV err view)

instance (ToJSON (g (First (Maybe err))), ToJSON (view g)) => ToJSON (ErrorV err view g)
instance (View view, FromJSON (g (First (Maybe err))), FromJSON (view g)) => FromJSON (ErrorV err view g)

deriving instance (Has' Semigroup (ErrorVK err v) (FlipAp g), View v) => Semigroup (ErrorV err v g)
deriving instance (Has' Semigroup (ErrorVK err v) (FlipAp g), View v) => Monoid (ErrorV err v g)
deriving instance (Has' Additive (ErrorVK err v) (FlipAp g), View v) => Additive (ErrorV err v g)
deriving instance (Has' Group (ErrorVK err v) (FlipAp g), View v) => Group (ErrorV err v g)
deriving instance (PositivePart (g (First (Maybe err))), PositivePart (v g)) => PositivePart (ErrorV err v g)

instance
  ( Semigroup (v Identity)
  , View v
  , QueryResult (v Proxy) ~ v Identity
  ) => Query (ErrorV err v Proxy) where
  type QueryResult (ErrorV err v Proxy) = ErrorV err v Identity
  crop (ErrorV s) (ErrorV r) = ErrorV $ crop s r

instance
  ( Semigroup (v Identity)
  , View v
  , QueryResult (v (Const ())) ~ v Identity
  ) => Query (ErrorV err v (Const ())) where
  type QueryResult (ErrorV err v (Const ())) = ErrorV err v Identity
  crop (ErrorV s) (ErrorV r) = ErrorV $ crop s r

instance
  ( Semigroup (v Identity)
  , View v
  , QueryResult (v (Const SelectedCount)) ~ v Identity
  ) => Query (ErrorV err v (Const SelectedCount)) where
  type QueryResult (ErrorV err v (Const SelectedCount)) = ErrorV err v Identity
  crop (ErrorV s) (ErrorV r) = ErrorV $ crop s r

instance
  ( View v
  , Has' Semigroup (ErrorVK err v) (FlipAp (Compose c (VesselLeafWrapper (QueryResult (Vessel (ErrorVK err v) g)))))
  , Query (Vessel (ErrorVK err v) g)
  ) => Query (ErrorV err v (Compose c g)) where
  type QueryResult (ErrorV err v (Compose c g)) = ErrorV err v (Compose c (VesselLeafWrapper (QueryResult (Vessel (ErrorVK err v) g))))
  crop (ErrorV s) (ErrorV r) = ErrorV $ crop s r

-- | The error part of the view will never be present
liftErrorV :: View v => v g -> ErrorV e v g
liftErrorV = ErrorV . singletonV ErrorVK_View

-- | The successful part of the view will never be present
failureErrorV :: e -> ErrorV e v Identity
failureErrorV = ErrorV . singletonV ErrorVK_Error . SingleV . Identity . First . Just

buildErrorV
  :: (View v, Monad m)
  => (v Proxy -> m (Either e (v Identity)))
  -> ErrorV e v Proxy
  -> m (ErrorV e v Identity)
buildErrorV f (ErrorV v) = case lookupV ErrorVK_View v of
  Nothing -> pure (ErrorV emptyV)
  Just v' -> f v' >>= \case
    Left err -> pure $ failureErrorV err
    Right val -> pure $ liftErrorV val

observeErrorV
  :: EmptyView v
  => ErrorV e v Identity
  -> Either e (v Identity)
observeErrorV (ErrorV v) = case lookupV ErrorVK_Error v of
  Nothing -> Right $ case lookupV ErrorVK_View v of
    Nothing -> emptyV
    Just v' -> v'
  Just err -> case lookupSingleV err of
    Nothing -> Right emptyV
    Just e -> Left e

unsafeProjectE
  :: ( EmptyView v
     )
  => QueryMorphism
       ()
       (ErrorV () v (Const SelectedCount))
unsafeProjectE = QueryMorphism
  { _queryMorphism_mapQuery = const (liftErrorV emptyV)
  , _queryMorphism_mapQueryResult = const ()
  }

unsafeProjectV
  :: (EmptyView v, QueryResult (v (Const SelectedCount)) ~ v Identity)
  => QueryMorphism
       (v (Const SelectedCount))
       (ErrorV () v (Const SelectedCount))
unsafeProjectV = QueryMorphism
  { _queryMorphism_mapQuery = liftErrorV
  , _queryMorphism_mapQueryResult = \r -> case observeErrorV r of
      Left _ -> emptyV
      Right r' -> r'
  }

newtype AuthMapV auth v g = AuthMapV { unAuthMapV :: SubVessel auth (ErrorV () v) g }
  deriving (Generic)

deriving instance (Ord auth, Eq (view g), Eq (g (First (Maybe ())))) => Eq (AuthMapV auth view g)

instance (Ord auth, ToJSON auth, ToJSON (g (First (Maybe ()))), ToJSON (view g)) => ToJSON (AuthMapV auth view g)
instance (Ord auth, FromJSON auth, View view, FromJSON (g (First (Maybe ()))), FromJSON (view g)) => FromJSON (AuthMapV auth view g)

deriving instance
  ( Ord auth
  , Has' Semigroup (ErrorVK () v) (FlipAp g)
  , View v
  ) => Semigroup (AuthMapV auth v g)

deriving instance
  ( Ord auth
  , Has' Semigroup (ErrorVK () v) (FlipAp g)
  , View v
  ) => Monoid (AuthMapV auth v g)

deriving instance
  ( Ord auth
  , Has' Group (ErrorVK () v) (FlipAp g)
  , View v
  ) => Group (AuthMapV auth v g)

deriving instance
  ( Ord auth
  , Has' Additive (ErrorVK () v) (FlipAp g)
  , View v
  ) => Additive (AuthMapV auth v g)

deriving instance (Ord auth, PositivePart (g (First (Maybe ()))), PositivePart (v g)) => PositivePart (AuthMapV auth v g)

instance (Ord auth, View v) => View (AuthMapV auth v)
instance (Ord auth, View v) => EmptyView (AuthMapV auth v) where
  emptyV = AuthMapV emptyV

instance
  ( Ord auth
  , Semigroup (v Identity)
  , View v
  , QueryResult (v Proxy) ~ v Identity
  ) => Query (AuthMapV auth v Proxy) where
  type QueryResult (AuthMapV auth v Proxy) = AuthMapV auth v Identity
  crop (AuthMapV s) (AuthMapV r) = AuthMapV $ crop s r

instance
  ( Ord auth
  , Semigroup (v Identity)
  , View v
  , QueryResult (v (Const ())) ~ v Identity
  ) => Query (AuthMapV auth v (Const ())) where
  type QueryResult (AuthMapV auth v (Const ())) = AuthMapV auth v Identity
  crop (AuthMapV s) (AuthMapV r) = AuthMapV $ crop s r

instance
  ( Ord auth
  , Semigroup (v Identity)
  , View v
  , QueryResult (v (Const SelectedCount)) ~ v Identity
  ) => Query (AuthMapV auth v (Const SelectedCount)) where
  type QueryResult (AuthMapV auth v (Const SelectedCount)) = AuthMapV auth v Identity
  crop (AuthMapV s) (AuthMapV r) = AuthMapV $ crop s r

instance
  ( Ord auth
  , View v
  , Has' Semigroup (ErrorVK () v) (FlipAp (Compose c (VesselLeafWrapper (QueryResult (Vessel (SubVesselKey auth (ErrorV () v)) g)))))
  , Query (Vessel (SubVesselKey auth (ErrorV () v)) g)
  ) => Query (AuthMapV auth v (Compose c g)) where
  type QueryResult (AuthMapV auth v (Compose c g)) = AuthMapV auth v (Compose c (VesselLeafWrapper (QueryResult (Vessel (SubVesselKey auth (ErrorV () v)) g))))
  crop (AuthMapV s) (AuthMapV r) = AuthMapV $ crop s r

-- TODO ORPHAN
instance Additive (g (f x)) => Additive (Compose g f x)

-- token is usually (Signed (AuthToken Identity))
-- user is usually (Id Account), iso to AuthToken Identity
handleAuthMapQuery
  :: (Monad m, Ord token, View v)
  => (token -> m (Maybe user))
  -- ^ How to figure out the identity corresponding to a token
  -> (v Proxy -> m (v Identity))
  -- ^ Handle the aggregate query for all identities
  -> AuthMapV token v Proxy
  -- ^ Private views parameterized by tokens
  -> m (AuthMapV token v Identity)
handleAuthMapQuery readToken handler (AuthMapV vt) = do
  let unfilteredVt = getSubVessel vt
      unvalidatedTokens = MMap.keys unfilteredVt
  validTokens <- Set.fromList <$> witherM (\t -> pure t <$ readToken t) unvalidatedTokens
  let filteredVt = MMap.intersectionWith const unfilteredVt (MMap.fromSet (\_ -> ()) validTokens)
      invalidTokens = MMap.fromSet (\_ -> failureErrorV ()) $
        Set.difference (Set.fromList unvalidatedTokens) validTokens
      v = condenseV filteredVt
  v' <- disperseV . fromMaybe emptyV <$> mapDecomposedV (buildErrorV (fmap Right . handler)) v
  -- The use of mapDecomposedV guarantees that the valid and invalid token sets are disjoint
  pure $ AuthMapV $ mkSubVessel $ MMap.unionWith const invalidTokens v'

authMapQueryMorphism
  :: (Ord token, View v)
  => token
  -> QueryMorphism
       (ErrorV () v (Const SelectedCount))
       (AuthMapV token v (Const SelectedCount))
authMapQueryMorphism token = QueryMorphism
  { _queryMorphism_mapQuery = AuthMapV . singletonSubVessel token
  , _queryMorphism_mapQueryResult = maybe emptyV id . lookupSubVessel token . unAuthMapV
  }

data AuthenticatedVKey public private (x :: (* -> *) -> *) where
  AuthenticatedVKey_Public :: AuthenticatedVKey public private public
  AuthenticatedVKey_Private :: AuthenticatedVKey public private private

deriveJSONGADT ''AuthenticatedVKey

instance GEq (AuthenticatedVKey public private) where
  geq = \case
    AuthenticatedVKey_Public -> \case
      AuthenticatedVKey_Public -> Just Refl
      AuthenticatedVKey_Private -> Nothing
    AuthenticatedVKey_Private -> \case
      AuthenticatedVKey_Public -> Nothing
      AuthenticatedVKey_Private -> Just Refl

instance GCompare (AuthenticatedVKey public private) where
  gcompare = \case
    AuthenticatedVKey_Public -> \case
      AuthenticatedVKey_Public -> GEQ
      AuthenticatedVKey_Private -> GLT
    AuthenticatedVKey_Private -> \case
      AuthenticatedVKey_Public -> GGT
      AuthenticatedVKey_Private -> GEQ

instance ArgDict c (AuthenticatedVKey public private) where
  type ConstraintsFor (AuthenticatedVKey public private) c = (c public, c private)
  argDict = \case
    AuthenticatedVKey_Public -> Dict
    AuthenticatedVKey_Private -> Dict

newtype AuthenticatedV public private g = AuthenticatedV
  { unAuthenticatedV :: Vessel (AuthenticatedVKey public private) g
  } deriving (Generic, Eq, ToJSON, FromJSON, Semigroup, Monoid, Group, Additive)

instance (View public, View private) => View (AuthenticatedV public private)

instance (View public, View private) => EmptyView (AuthenticatedV public private) where
  emptyV = AuthenticatedV emptyV

instance
  ( Semigroup (public Identity)
  , Semigroup (private Identity)
  , View public, View private
  , QueryResult (private Proxy) ~ private Identity
  ) => Query (AuthenticatedV public private Proxy) where
  type QueryResult (AuthenticatedV public private Proxy) = AuthenticatedV public private Identity
  crop (AuthenticatedV s) (AuthenticatedV r) = AuthenticatedV $ crop s r

instance
  ( Semigroup (public Identity)
  , Semigroup (private Identity)
  , View public, View private
  , QueryResult (private (Const ())) ~ private Identity
  ) => Query (AuthenticatedV public private (Const ())) where
  type QueryResult (AuthenticatedV public private (Const ())) = AuthenticatedV public private Identity
  crop (AuthenticatedV s) (AuthenticatedV r) = AuthenticatedV $ crop s r

instance
  ( Semigroup (public Identity)
  , Semigroup (private Identity)
  , View public, View private
  , QueryResult (private (Const SelectedCount)) ~ private Identity
  ) => Query (AuthenticatedV public private (Const SelectedCount)) where
  type QueryResult (AuthenticatedV public private (Const SelectedCount)) = AuthenticatedV public private Identity
  crop (AuthenticatedV s) (AuthenticatedV r) = AuthenticatedV $ crop s r

instance
  ( View public, View private
  , Semigroup (private (Compose c (VesselLeafWrapper (QueryResult (Vessel (AuthenticatedVKey public private) g)))))
  , Semigroup (public (Compose c (VesselLeafWrapper (QueryResult (Vessel (AuthenticatedVKey public private) g)))))
  , Query (Vessel (AuthenticatedVKey public private) g)
  ) => Query (AuthenticatedV public private (Compose c (g :: * -> *))) where
  type QueryResult (AuthenticatedV public private (Compose c g)) = AuthenticatedV public private
         (Compose c (VesselLeafWrapper (QueryResult (Vessel (AuthenticatedVKey public private) g))))
  crop (AuthenticatedV s) (AuthenticatedV r) = AuthenticatedV $ crop s r

-- token is usually (Signed (AuthToken Identity))
-- user is usually (Id Account), iso to AuthToken Identity
handleAuthenticatedQuery'
  :: (Monad m, View public, View private)
  => (public Proxy -> m (public Identity))
  -- ^ Handle the aggregate public query
  -> (private Proxy -> m (private Identity))
  -- ^ Handle the aggregate private query for all identities
  -> AuthenticatedV public private Proxy
  -- ^ Private views parameterized by tokens
  -> m (AuthenticatedV public private Identity)
handleAuthenticatedQuery' public private (AuthenticatedV q) = fmap AuthenticatedV $ buildV q $ \case
  AuthenticatedVKey_Public -> public
  AuthenticatedVKey_Private -> private

handleAuthenticatedQuery
  :: (Monad m, Ord token, View public, View private)
  => (token -> m (Maybe user))
  -> (public Proxy -> m (public Identity))
  -> (private Proxy -> m (private Identity))
  -> AuthenticatedV public (AuthMapV token private) Proxy
  -> m (AuthenticatedV public (AuthMapV token private) Identity)
handleAuthenticatedQuery readToken public private =
  handleAuthenticatedQuery' public (handleAuthMapQuery readToken private)

privateQueryMorphism
  :: ( EmptyView private, QueryResult (private (Const SelectedCount)) ~ private Identity
     )
  => QueryMorphism
       (private (Const SelectedCount))
       (AuthenticatedV public private (Const SelectedCount))
privateQueryMorphism = QueryMorphism
  { _queryMorphism_mapQuery = AuthenticatedV . singletonV AuthenticatedVKey_Private
  , _queryMorphism_mapQueryResult = maybe emptyV id . lookupV AuthenticatedVKey_Private . unAuthenticatedV
  }

publicQueryMorphism
  :: ( EmptyView public, QueryResult (public (Const SelectedCount)) ~ public Identity
     )
  => QueryMorphism
       (public (Const SelectedCount))
       (AuthenticatedV public private (Const SelectedCount))
publicQueryMorphism = QueryMorphism
  { _queryMorphism_mapQuery = AuthenticatedV . singletonV AuthenticatedVKey_Public
  , _queryMorphism_mapQueryResult = maybe emptyV id . lookupV AuthenticatedVKey_Public . unAuthenticatedV
  }
