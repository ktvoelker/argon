
-- Unlike the other Declare modules, this one is not used by Config.
-- This module provides accessor functions for consumers of the Config.
module Declare.Access where

import Declare

cSpace :: (RefSpace a) => Config -> a -> Workspace
cSpace c sr = cSpaces c ! getSpaceRef sr

cTileRefs :: Config -> [TileRef]
cTileRefs c = concatMap (keys . laTiles . spLayout) $ elems $ cSpaces c

sameSpace :: (RefSpace a, RefSpace b) => a -> b -> Bool
sameSpace a b = getSpaceRef a == getSpaceRef b

