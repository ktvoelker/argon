
module Declare.Common (
    module Declare.Common,
    module Types
  ) where

import Types

data Table t = Table
  { taRows :: [Span t Y]
  , taCols :: [Span t X]
  } deriving (Eq, Ord, Show)

data Exec = Exec
  { exProg :: String
  , exArgs :: [String]
  } deriving (Eq, Ord, Show)

