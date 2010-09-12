
module Declare.Workspace where

import Declare.Layout
import Declare.Statusbar
import Types

data Workspace = Workspace
  { spLayout    :: Layout Pix TileRef
  , spStatus    :: Statusbar
  , spStartTile :: TileRef
  } deriving (Eq, Ord, Show)

