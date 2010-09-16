
module Tile where

import Focus
import State
import Types

import Graphics.X11

modifyTileFocus :: (Dequeue Window -> Dequeue Window) -> TileRef -> X11State ()
modifyTileFocus f tr = modifyTileWindows f tr >> updateX11Focus

nextWin, prevWin :: TileRef -> X11State ()

nextWin = modifyTileFocus $
  \q -> case extract q of
    Nothing      -> q
    Just (x, q') -> insert x q'

prevWin = modifyTileFocus $
  \q -> let xs = toList q in fromList $ last xs : init xs

