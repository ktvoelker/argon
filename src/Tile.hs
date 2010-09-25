
module Tile (nextWin, prevWin) where

import Focus
import State
import Types

import Graphics.X11

nextWin, prevWin :: TileRef -> X11State ()

nextWin = rotateWin popFront pushBack

prevWin = rotateWin popBack pushFront

rotateWin
  :: (DQ Window -> (Maybe Window, DQ Window))
  -> (DQ Window -> Window -> DQ Window)
  -> TileRef
  -> X11State ()
rotateWin pop push tr = do
  modifyTileWindows f tr
  refreshSpace tr
  where
    f q = case pop q of
      (Nothing, _) -> q
      (Just x, q') -> push q' x

