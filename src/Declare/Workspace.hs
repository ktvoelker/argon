
module Declare.Workspace where

import Declare.Attract
import Declare.Layout
import Declare.Statusbar
import Types

data Workspace = Workspace
  { spLayout    :: Layout Pix
  , spStatus    :: Statusbar
  , spAttracts  :: [(Attract, Maybe Name)]
  , spStartTile :: Maybe Name
  } deriving (Eq, Ord, Show)

