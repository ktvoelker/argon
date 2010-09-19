
module Command where

import Debug
import Declare
import Exec
import Focus
import History.Tile
import State
import Tile
import Types
import X11

import Graphics.X11.Xlib.Extras

runCommand :: Command -> X11State ()

runCommand cmd = do
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
    _               -> return ()

