
module Declare.Layout where

import Declare.Common
import Types

data Tile t = Tile
  { tiPos      :: XYPosn t
  , tiSpan     :: XYSpan t
  } deriving (Eq, Ord, Show)

data Layout t = Layout
  { laTable     :: Table t
  , laTiles     :: Map Name (Tile Cel)
  } deriving (Eq, Ord, Show)

emptyLayout :: Layout t
emptyLayout = Layout
  { laTable = Table { taRows = [], taCols = [] }
  , laTiles = empty
  }

