
module History.Tile
  ( TileHistory, setFocusTile, tileHistBack, tileHistFwd
  ) where

import Fields
import History
import State
import Types

type TileHistory = History TileRef

setFocusTile :: TileRef -> X11State ()
setFocusTile tr = do
  modifyWorld $ $(upd 'wHistory) $ histGo tr
  setFocusTile' tr

setFocusTile' :: TileRef -> X11State ()
setFocusTile' tr = do
  modifyWorld $ $(upd 'wFocus) ctr
  modifyLocalFocus ctr tr
  where
    ctr = const tr

tileHistBack, tileHistFwd :: X11State ()
tileHistBack = tileHistMove histBack
tileHistFwd  = tileHistMove histFwd

tileHistMove :: (TileHistory -> (Maybe TileRef, TileHistory)) -> X11State ()
tileHistMove f = do
  wo <- getWorld
  let h = wHistory wo
  let (tr, h') = f h
  putWorld wo { wHistory = h' }
  maybe (return ()) setFocusTile' tr
  updateX11Focus

