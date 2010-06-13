
module Declare.Common (
    module Declare.Common,
    module Types
  ) where

import Types

data Table = Table
  { rows :: [Nat]
  , cols :: [Nat]
  } deriving (Eq, Ord, Show)

