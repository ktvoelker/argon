
module Ref where

import Prelude (Eq(), Ord(), Show(), Maybe(), String, id)

newtype SpaceRef = SpaceRef
  { srSpace :: String
  } deriving (Eq, Ord, Show)

data TileRef = TileRef
  { trSpace :: SpaceRef
  , trTile  :: Maybe String
  } deriving (Eq, Ord, Show)

data StatusRef = StatusRef
  { strSpace :: SpaceRef
  , strTile  :: String
  } deriving (Eq, Ord, Show)

class RefSpace a where
  getSpaceRef :: a -> SpaceRef

instance RefSpace SpaceRef where
  getSpaceRef = id

instance RefSpace TileRef where
  getSpaceRef = trSpace

instance RefSpace StatusRef where
  getSpaceRef = strSpace

