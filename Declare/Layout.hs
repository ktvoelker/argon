
module Declare.Layout where

import Declare.Common

data Tile = Tile
  { name :: Name
  , pos  :: (Nat, Nat)
  , span :: (Nat, Nat)
  } deriving (Eq, Ord, Show)

data Layout = Layout
  { table :: Table
  , tiles :: [Tile]
  } deriving (Eq, Ord, Show)

