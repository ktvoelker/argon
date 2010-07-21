
module Declare.Workspace where

import Declare.Attract
import Declare.Layout    (Layout)
import Declare.Statusbar (Statusbar)
import Types

data Workspace = Workspace
  { layout    :: Layout
  , status    :: Statusbar
  , attracts  :: [(Attract, Maybe Name)]
  , startTile :: Maybe Name
  } deriving (Eq, Ord, Show)

