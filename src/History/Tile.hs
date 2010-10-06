
module History.Tile
  ( TileHistory, setFocusTile, tileHistBack, tileHistFwd
  ) where

import Fields
import History
import State
import Types

import Control.Monad

type TileHistory = History TileRef

setFocusTile :: TileRef -> X11State ()
setFocusTile tr = do
  getFocusTileM >>= modifyWorld . $(upd 'wHistory) . histGo
  setFocusTile' tr

setFocusTile' :: TileRef -> X11State ()
setFocusTile' tr = do
  when (tileIsFloat tr) $ setShowFloat tr True
  modifyWorld $ $(upd 'wFocus) ctr
  modifyLocalFocus ctr tr
  where
    ctr = const tr

tileHistBack, tileHistFwd :: X11State ()
tileHistBack = tileHistMove histBack
tileHistFwd  = tileHistMove histFwd

tileHistMove
  :: (TileRef -> TileHistory -> (Maybe TileRef, TileHistory))
  -> X11State ()
tileHistMove f = do
  wo <- getWorld
  let tr = getFocusTile wo
  let h = wHistory wo
  let (tr', h') = f tr h
  putWorld wo { wHistory = h' }
  maybe (return ()) setFocusTile' tr'
  updateX11Focus

