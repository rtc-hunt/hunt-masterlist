module Frontend.Patch where

import Data.Patch.Class
import Data.Patch.MapWithPatchingMove

import Control.Lens ((<&>))
import Control.Lens.TH (makeWrapped)
import Data.Align (align)
import Data.Foldable (toList)
import Data.Function
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Semigroup (Semigroup (..), First (..))
import Data.Monoid.DecidablyEmpty
import Data.Set (Set)
import qualified Data.Set as Set
import Data.These (These (..))

-- | Create a 'PatchMapWithPatchingMove' that, if applied to the first 'Map' provided,
-- will produce the second 'Map'.
-- Note: this will never produce a patch on a value.
patchThatChangesMapWith
  :: forall k p f
  .  (Ord k, Ord f) -- , Monoid p)
  => (PatchTarget p -> f) -> (PatchTarget p -> PatchTarget p -> p) -> Map k (PatchTarget p) -> Map k (PatchTarget p) -> PatchMapWithPatchingMove k p
patchThatChangesMapWith eqv diff oldByIndex newByIndex = patch
  where invert :: Map k (PatchTarget p) -> Map f (First (PatchTarget p), Set k)
        invert = Map.fromListWith (<>) . fmap (\(k, v) -> (eqv v, (First v, Set.singleton k))) . Map.toList
        -- In the places where we use unionDistinct, a non-distinct key indicates a bug in this function
        unionDistinct :: forall k' v'. Ord k' => Map k' v' -> Map k' v' -> Map k' v'
        unionDistinct = Map.unionWith (error "patchThatChangesMap: non-distinct keys")
        unionPairDistinct :: (Map k (From k v), Map k (To k)) -> (Map k (From k v), Map k (To k)) -> (Map k (From k v), Map k (To k))
        unionPairDistinct (oldFroms, oldTos) (newFroms, newTos) = (unionDistinct oldFroms newFroms, unionDistinct oldTos newTos)
        -- Generate patch info for a single value
        -- Keys that are found in both the old and new sets will not be patched
        -- Keys that are found in only the old set will be moved to a new position if any are available; otherwise they will be deleted
        -- Keys that are found in only the new set will be populated by moving an old key if any are available; otherwise they will be inserted
        patchSingleValue :: (PatchTarget p, PatchTarget p) -> Set k -> Set k -> (Map k (From k p), Map k (To k))
        patchSingleValue (oldv, newv) oldKeys newKeys = foldl' unionPairDistinct mempty $ align (toList $ oldKeys `Set.difference` newKeys) (toList $ newKeys `Set.difference` oldKeys) <&> \case
          This oldK -> (mempty, Map.singleton oldK Nothing) -- There's nowhere for this value to go, so we know we are deleting it
          That newK -> (Map.singleton newK $ From_Insert newv, mempty) -- There's nowhere fo this value to come from, so we know we are inserting it
          These oldK newK -> (Map.singleton newK $ From_Move oldK (diff oldv newv), Map.singleton oldK $ Just newK)
        -- Run patchSingleValue on a These.  Missing old or new sets are considered empty
        patchSingleValueThese :: f -> These (First (PatchTarget p), Set k) (First (PatchTarget p), Set k) -> (Map k (From k p), Map k (To k))
        patchSingleValueThese fv = \case
          This (v, oldKeys) -> patchSingleValue (getFirst v, getFirst v) oldKeys mempty
          That (v, newKeys) -> patchSingleValue (getFirst v, getFirst v) mempty newKeys
          These (oldv, oldKeys) (newv, newKeys) -> patchSingleValue (getFirst oldv, getFirst newv) oldKeys newKeys
        -- Generate froms and tos for all values, then merge them together
        (froms, tos) = foldl' unionPairDistinct mempty $ Map.mapWithKey patchSingleValueThese $ align (invert oldByIndex) (invert newByIndex)
        patch = unsafePatchMapWithPatchingMove $ align froms tos <&> \case
          This from -> NodeInfo from Nothing -- Since we don't have a 'to' record for this key, that must mean it isn't being moved anywhere, so it should be deleted.
          That to -> NodeInfo From_Delete to -- Since we don't have a 'from' record for this key, it must be getting deleted
          These from to -> NodeInfo from to

