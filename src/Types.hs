
module Types
  ( module Types
  , module Maths
  , module Ref
  , module Data.Dequeue
  , module Data.Map
  , module Data.Queue.Class
  , module Data.Queue.PQueue
  , module Data.Queue.Queue
  , module Data.Queue.Stack
  , module Data.Set
  , module Prelude
  ) where

import Maths

import Ref
  ( SpaceRef()
  , TileRef()
  , StatusRef()
  , RefSpace(..)
  , getFloatRef
  )

import qualified Ref

import Data.Dequeue hiding (null, empty, fromList, length, last)
import Data.Map
  ( Map, (!), keys, elems, adjust, unions, lookup, findWithDefault
  , filterWithKey, mapWithKey, union, mapKeys
  )
import Data.Maybe
import Data.Queue.Class hiding
  ( null, delete, empty, fromList, insert, singleton, size, toList
  )
import Data.Queue.PQueue
import Data.Queue.Queue
import Data.Queue.Stack
import Data.Set (Set, delete)

import qualified Data.Dequeue as DD
import qualified Data.List as DL
import qualified Data.Map as DM
import qualified Data.Queue.Class as DQ
import qualified Data.Set as DS

import Prelude hiding (filter, lookup, null, span, length, last)

impossible :: a
impossible = error "Impossible!"

tileIsFloat :: TileRef -> Bool
tileIsFloat = not . isJust . Ref.trTile

class Collection a where
  type Entry a
  type Key a

  fromList :: (Ord (Key a)) => [Entry a] -> a
  toList :: a -> [Entry a]
  empty :: a
  singleton :: Entry a -> a

  null :: a -> Bool
  null = DL.null . toList

  insert :: (Ord (Key a)) => Entry a -> a -> a
  insert v = fromList . (v :) . toList

  size :: a -> Int
  size = DL.length . toList

  filter :: (Ord (Key a)) => (Entry a -> Bool) -> a -> a
  filter p = fromList . DL.filter p . toList

  member :: (Ord (Key a), Eq (Entry a)) => Entry a -> a -> Bool
  member x = not . null . filter (== x)

maximumBy :: (Collection a)
          => (Entry a -> Entry a -> Ordering) -> a -> Entry a
maximumBy f = DL.maximumBy f . toList

instance Collection [e] where
  type Entry [e] = e
  type Key [e] = e
  null = DL.null
  empty = []
  fromList = id
  insert = (:)
  singleton = (: [])
  size = DL.length
  toList = id
  filter = DL.filter
  member = DL.elem

instance Collection (Map k v) where
  type Entry (Map k v) = (k, v)
  type Key (Map k v)   = k
  null = DM.null
  empty = DM.empty
  fromList = DM.fromList
  insert = uncurry DM.insert
  singleton = uncurry DM.singleton
  size = DM.size
  toList = DM.toList
  filter = DM.filterWithKey . curry

instance Collection (Set e) where
  type Entry (Set e) = e
  type Key (Set e) = e
  null = DS.null
  empty = DS.empty
  fromList = DS.fromList
  insert = DS.insert
  singleton = DS.singleton
  size = DS.size
  toList = DS.toList
  filter = DS.filter
  member = DS.member

type DQ e = BankersDequeue e
 
instance (Ord e) => Collection (BankersDequeue e) where
  type Entry (BankersDequeue e) = e
  type Key (BankersDequeue e) = e
  null = DD.null
  empty = DD.empty
  fromList = DD.fromList
  insert = flip DD.pushFront
  singleton = pushFront empty
  size = DD.length
  toList q = case popFront q of
    (Nothing, _) -> []
    (Just x, q') -> x : toList q'

instance (Ord e) => Collection (PQueue e) where
  type Entry (PQueue e) = e
  type Key (PQueue e) = e
  null = DQ.null
  empty = DQ.empty
  fromList = DQ.fromList
  insert = DQ.insert
  singleton = DQ.singleton
  size = DQ.size
  toList = DQ.toList
 
instance (Ord e) => Collection (Queue e) where
  type Entry (Queue e) = e
  type Key (Queue e) = e
  null = DQ.null
  empty = DQ.empty
  fromList = DQ.fromList
  insert = DQ.insert
  singleton = DQ.singleton
  size = DQ.size
  toList = DQ.toList
 
instance (Ord e) => Collection (Stack e) where
  type Entry (Stack e) = e
  type Key (Stack e) = e
  null = DQ.null
  empty = DQ.empty
  fromList = DQ.fromList
  insert = DQ.insert
  singleton = DQ.singleton
  size = DQ.size
  toList = DQ.toList

instance (Show e) => Show (Queue e) where
  showsPrec n q = ("fromList " ++) . showsPrec n (DQ.toList q)

instance (Show e) => Show (Stack e) where
  showsPrec n q = ("fromList " ++) . showsPrec n (DQ.toList q)

