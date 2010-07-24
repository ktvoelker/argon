
module Declare.Workspace where

import Declare.Attract
import Declare.Layout
import Declare.Statusbar
import Types

data Workspace = Workspace
  { spLayout  :: Layout Pix
  , status    :: Statusbar
  , attracts  :: [(Attract, Maybe Name)]
  , startTile :: Maybe Name
  } deriving (Eq, Ord, Show)

