
module Tile (nextWin, prevWin, removeWin, addWin) where

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

removeWin :: TileRef -> X11State (Maybe Window)
removeWin tr = do
  wo <- getWorld
  let win = first $ getTileWindows wo tr
  modifyTileWindows (snd . popFront) tr
  return win

addWin :: TileRef -> Window -> X11State ()
addWin tr win = modifyTileWindows (flip pushFront win) tr

