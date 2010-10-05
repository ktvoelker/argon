
module Command (runCommand) where

import Debug
import Declare
import Exec
import Fields
import Focus
import History.Tile
import State
import Tile
import Types
import X11

import Graphics.X11.Xlib.Extras

runCommand, runCommand' :: Command -> X11State ()

runCommand cmd = do
  wo <- getWorld
  case wMode wo of
    MNormal -> runCommand' cmd
    _       -> return ()

runCommand' cmd = do
  dprint cmd
  case cmd of
    CQuit           -> quitState
    (CSeq xs)       -> mapM_ runCommand xs
    (CFocusDir dir) -> focusDir dir
    (CExec x)       -> exec x
    CNextWin        -> getFocusTileM >>= nextWin
    CPrevWin        -> getFocusTileM >>= prevWin
    CKill           -> do
      d <- getDisplay
      w <- getFocusWindow
      liftIO $ killClient d w
      return ()
    (CSpace sr)     -> do
      wo <- getWorld
      let old = getFocusTile wo
      let new = getLocalFocus wo sr
      setFocusTile new
      refreshSpace old
      refreshSpace new
    CHistBack       -> tileHistBack
    CHistFwd        -> tileHistFwd
    CFocusFloat     -> getFocusTileM >>= setFocusTile . getFloatRef
    CKeyMode mr     -> modifyWorld $ $(upd 'wKeyMode) $ const mr
    CPut cmd        -> do
      from <- getFocusTileM
      runCommand cmd
      to <- getFocusTileM
      removeWin from >>= maybe (return ()) (addWin to)
    _               -> return ()

