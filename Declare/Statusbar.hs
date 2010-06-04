
module Declare.Statusbar where

import Declare.Common
import Declare.Layout (Layout)

data Exec = Exec
  { prog :: String
  , args :: [String]
  , freq :: Nat
  } deriving (Eq, Ord, Show)

data Statusbar = Statusbar
  { name   :: Name
  , layout :: Layout
  , execs  :: Map Name Exec
  } deriving (Eq, Ord, Show)

