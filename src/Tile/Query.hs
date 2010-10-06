
module Tile.Query where

import Types

type TileQueryResult = [(TileRef, Provenance)]

data Provenance = PNone | PBack | PFwd deriving (Enum, Eq, Ord, Show)

evalTileQuery :: TileQuery -> World -> TileQueryResult
-- TODO
evalTileQuery _ _ = []

