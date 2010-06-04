
module Declare.Common where

type Nat = Int

type Name = String

type Assoc k v = [(k, v)]

data Table = Table
  { rows :: [Nat]
  , cols :: [Nat]
  } deriving (Eq, Ord, Show)

