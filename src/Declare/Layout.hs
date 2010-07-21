
module Declare.Layout where

import Declare.Common

data Tile = Tile
  { pos      :: (Nat, Nat)
  , span     :: (Nat, Nat)
  } deriving (Eq, Ord, Show)

data Layout = Layout
  { table     :: Table
  , tiles     :: Map Name Tile
  } deriving (Eq, Ord, Show)

