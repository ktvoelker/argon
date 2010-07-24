
module Declare.Layout where

import Declare.Common
import Types

data Tile t = Tile
  { pos      :: XY Posn t
  , span     :: XY Span t
  } deriving (Eq, Ord, Show)

data Layout t = Layout
  { table     :: Table t
  , tiles     :: Map Name (Tile Cel)
  } deriving (Eq, Ord, Show)

emptyLayout :: Layout t
emptyLayout = Layout
  { table = Table { rows = [], cols = [] }
  , tiles = empty
  }

