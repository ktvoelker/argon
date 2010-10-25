
module Declare.Workspace where

import Declare.Layout
import Types

data Workspace = Workspace
  { spLayout    :: Layout Pix TileRef
  , spStartTile :: TileRef
  } deriving (Eq, Ord, Show)

