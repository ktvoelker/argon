
module Declare.Common (
    module Declare.Common,
    module Types
  ) where

import Types

data Table t = Table
  { rows :: [Span t Y]
  , cols :: [Span t X]
  } deriving (Eq, Ord, Show)

