
module Tile where

import Focus
import State
import Types

import Graphics.X11

modifyTileFocus
  :: (BankersDequeue Window -> BankersDequeue Window)
  -> TileRef
  -> X11State ()
modifyTileFocus f tr = modifyTileWindows f tr >> updateX11Focus

nextWin, prevWin :: TileRef -> X11State ()

nextWin = undefined
-- TODO

prevWin = undefined
-- TODO

