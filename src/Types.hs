
module Types (
    module Types,
    module Maths,
    module Data.Map,
    module Data.Queue.Class,
    module Data.Queue.PQueue,
    module Data.Queue.Queue,
    module Data.Queue.Stack
  ) where

import Maths

import Data.Map hiding (
    keys, null, empty, fromList, insert, singleton, size, toList, map
  )
import Data.Queue.Class hiding (
    null, delete, empty, fromList, insert, singleton, size, toList
  )
import Data.Queue.PQueue
import Data.Queue.Queue
import Data.Queue.Stack

import qualified Data.List as DL
import qualified Data.Map as DM
import qualified Data.Queue.Class as DQ

type Nat = Int

type Name = String

class Collection a where
  type Entry a
  type Key a
  null      :: a -> Bool
  empty     :: a
  fromList  :: (Ord (Key a)) => [Entry a] -> a
  insert    :: (Ord (Key a)) => Entry a -> a -> a
  singleton :: Entry a -> a
  size      :: a -> Int
  toList    :: a -> [Entry a]

instance Collection [e] where
  type Entry [e] = e
  type Key [e] = e
  null = DL.null
  empty = []
  fromList = id
  insert = (:)
  singleton = (: [])
  size = length
  toList = id

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

