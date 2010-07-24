
module Declare.Statusbar where

import Declare.Common
import Declare.Layout
import Types

data Exec = Exec
  { prog :: String
  , args :: [String]
  , wait :: SecSpan
  } deriving (Eq, Ord, Show)

data Statusbar = Statusbar
  { stLayout :: Layout Chr
  , execs    :: Map Name Exec
  } deriving (Eq, Ord, Show)

emptyStatusbar :: Statusbar
emptyStatusbar = Statusbar
  { stLayout = emptyLayout
  , execs    = empty
  }

