
module Declare.Layout where

import Declare.Common
import Types

data Tile t = Tile
  { tiPos      :: XYPosn t
  , tiSpan     :: XYSpan t
  } deriving (Eq, Ord, Show)

data Layout t r = Layout
  { laTable     :: Table t
  , laTiles     :: Map r (Tile Cel)
  } deriving (Eq, Ord, Show)

emptyLayout :: Layout t r
emptyLayout = Layout
  { laTable = Table { taRows = [], taCols = [] }
  , laTiles = empty
  }

