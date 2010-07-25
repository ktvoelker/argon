
module Declare.Statusbar where

import Declare.Common
import Declare.Layout
import Types

data Statusbar = Statusbar
  { stLayout :: Layout Chr
  , stExecs  :: Map Name (Exec, SecSpan)
  } deriving (Eq, Ord, Show)

emptyStatusbar :: Statusbar
emptyStatusbar = Statusbar
  { stLayout = emptyLayout
  , stExecs  = empty
  }

