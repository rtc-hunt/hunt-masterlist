{-# Language FlexibleInstances #-}
{-# Language FunctionalDependencies #-}
{-# Language RankNTypes #-}
{-# Language GADTs #-}
module Data.Vessel.Path where

import Control.Monad
import Control.Monad.Fix
import qualified Data.Dependent.Map.Monoidal as MonoidalDMap
import Data.GADT.Compare
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Map.Monoidal
import Data.Semigroup (First(..))
import Data.Set (Set)
import Data.Vessel
import Data.Vessel.Map
import Reflex
import Rhyolite.Frontend.App
import Rhyolite.SemiMap (SemiMap, getComplete)

-- * Stuff that can be upstreamed

-- | A (Path v w w' v') consists of maps in opposite directions:
--
--  >        v ---> w
--  > Maybe v' <--- w'
--
--  If we think of v / v' as variants of a "small" structure,
--  and w / w' as variants of a "large" structure,
--  this encodes how to on the one hand include v inside a
--  larger structure of type w,
--  and how to (potentially) extract a value of type v'
--  from a structure of type w'.
--
--  Formally, these are arrows in the product category of
--  Hask and the Kleisli category of Maybe.
data Path v w w' v' = Path { _path_to :: v -> w, _path_from :: w' -> Maybe v' }

(~>) :: Path b c c' b' -> Path a b b' a' -> Path a c c' a'
Path to from ~> Path to' from' = Path (to . to') (from' <=< from)

idP :: Path a a b b
idP = Path id pure

preMap :: (a -> b) -> Path a b x x
preMap f = Path f pure

postMap :: (a' -> Maybe b') -> Path x x a' b'
postMap f = Path id f

class Keyed k a b b' a'  | k b -> a, k b' -> a' where
  key :: k -> Path a b b' a'

class SetKeyed k a b b' a'  | k b -> a, k b' -> a' where
  keys :: Set k -> Path a b b' a'

instance (GCompare k, View v) => Keyed (k v) (v g) (Vessel k g) (Vessel k g') (v g') where
  key = vesselP

instance (Ord k, View v) => Keyed k (v g) (SubVessel k v g) (SubVessel k v g') (v g') where
  key = subVesselP

instance (Ord k) => Keyed k (g v) (MapV k v g) (MapV k v g') (g' v) where
  key = mapVP

instance (GCompare k) => Keyed (k a) (g (v a)) (DMapV k v g) (DMapV k v g') (g' (v a)) where
  key = dmapVP

instance (Ord k, Applicative g') => SetKeyed k (g v) (MapV k v g) (MapV k v g') (g' (Map k v)) where
  keys = mapVSetP

vesselP :: (GCompare k, View v) => k v -> Path (v g) (Vessel k g) (Vessel k g') (v g')
vesselP k = Path { _path_to = singletonV k, _path_from = lookupV k }

subVesselP :: (Ord k, View v) => k -> Path (v g) (SubVessel k v g) (SubVessel k v g') (v g')
subVesselP k = Path { _path_to = singletonSubVessel k, _path_from = lookupSubVessel k }

mapVP :: (Ord k) => k -> Path (g v) (MapV k v g) (MapV k v g') (g' v)
mapVP k = Path { _path_to = singletonMapV k, _path_from = lookupMapV k }

mapVSetP :: (Ord k, Applicative g') => Set k -> Path (g v) (MapV k v g) (MapV k v g') (g' (Map k v))
mapVSetP ks = Path
  { _path_to = \g -> MapV (MonoidalMap (Map.fromSet (const g) ks))
  , _path_from = Just . sequenceA . flip Map.restrictKeys ks . getMonoidalMap . unMapV
  }

dmapVP :: (GCompare k) => k a -> Path (g (v a)) (DMapV k v g) (DMapV k v g') (g' (v a))
dmapVP k = Path
  { _path_to = singletonDMapV k
  , _path_from = lookupDMapV k
  }

identityVP :: Path (g a) (IdentityV a g) (IdentityV a g') (g' a)
identityVP = Path
  { _path_to = IdentityV
  , _path_from = Just . unIdentityV
  }

singleVP :: (Traversable g', Functor g) => Path (g a) (SingleV a g) (SingleV a g') (g' a)
singleVP = Path
  { _path_to = SingleV . fmap (First . Just)
  , _path_from = sequenceA . fmap getFirst . unSingleV
  }

-- ** DMapV

singletonDMapV :: k a -> g (v a) -> DMapV k v g
singletonDMapV k v = DMapV $ MonoidalDMap.singleton k (Compose v)

lookupDMapV :: GCompare k => k a -> DMapV k v g -> Maybe (g (v a))
lookupDMapV k (DMapV m) = getCompose <$> MonoidalDMap.lookup k m

-- * Stuff that will end up in rhyolite

semiMapP :: (Traversable f) => Path x x (f (SemiMap k v)) (f (Map k v))
semiMapP = postMap (traverse (fmap getMonoidalMap . getComplete))

semiMapsP :: (Traversable f) => Path x x (f (Map k (SemiMap k' v))) (f (Map k (Map k' v)))
semiMapsP = postMap (traverse (Just . Map.mapMaybe (fmap getMonoidalMap . getComplete)))

-- merely a formality at this point
type FullPath a v b = Path (Const SelectedCount a) (v (Const SelectedCount)) (v Identity) (Identity b)


watch :: forall t v a b m.
  ( Reflex t
  , MonadQuery t (v (Const SelectedCount)) m
  , QueryResult (v (Const SelectedCount)) ~ v Identity
  , MonadHold t m
  , MonadFix m
  , Eq (v Identity)
  )
  => Dynamic t (FullPath a v b)
  -> m (Dynamic t (Maybe b))
watch pathDyn = do
  r <- watchViewSelector . ffor pathDyn $ \path -> _path_to path (Const 1)
  return $ fmap (fmap runIdentity) . _path_from <$> pathDyn <*> r
