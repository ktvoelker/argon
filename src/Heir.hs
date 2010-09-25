
module Heir (Heir(), mkHeir, heirLookup) where

import Types hiding (union, unions)

import Data.Maybe
import Data.Set (fold, union, unions, (\\))
import qualified Data.Set as DS

{- A multiple-inheritance heirarchy of nodes.
 - k: the type of node identifiers
 - v: the type of node contents
 - c: the type of combinations of nodes' contents
 -}
data Heir k v c =
  Heir
  { heir :: Map k ([k], v)
  , zero :: c
  , comb :: v -> c -> c
  }

instance (Show k, Show v) => Show (Heir k v c) where
  show = show . heir

mkHeir :: Map k ([k], v) -> c -> (v -> c -> c) -> Heir k v c
mkHeir = Heir

parentSet :: (Ord k) => k -> Heir k v c -> Set k
parentSet k h = fromList $ fromMaybe [] $ fmap fst $ lookup k $ heir h

ancestorSet :: (Ord k) => k -> Heir k v c -> Set k
ancestorSet k h = f empty $ singleton k
  where
    f acc ks = if null ks then acc else f acc' ks'
      where
        acc' = union acc ks
        ks'  = fold union empty (DS.map (flip parentSet h) ks) \\ acc'

contents :: (Ord k) => k -> Heir k v c -> Maybe v
contents k h = fmap snd $ lookup k $ heir h

heirLookup :: (Ord k) => k -> Heir k v c -> c
heirLookup k h =
  foldr (comb h) (zero h)
  $ catMaybes
  $ map (flip contents h)
  $ toList
  $ ancestorSet k h

