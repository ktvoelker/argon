
module Event.Structure where

import Attract
import Declare
import Declare.Access
import Debug
import Event.Default
import Event.Listen
import Fields
import Layout
import Maths.Unsafe
import State
import Types
import X11

import Graphics.X11
import Graphics.X11.Xlib.Extras

mapRequestHandler, destroyWindowHandler :: EventHandler

mapRequestHandler e = do
  debug "Map request!"
  wo <- getWorld
  -- Add standard event handlers.
  lift $ lift $ addStdEvents win
  -- Find out where the window belongs.
  tr <- attract win
  debug "Destination tile:"
  dprint tr
  -- Add the window to the tile queue.
  modifyTileWindows (insert win) tr
  -- Check if the tile is the floating tile.
  let isFloat = tileIsFloat tr
  -- Position the window.
  if isFloat
     then float tr
     else tile tr
  -- If the new window is floating on the focused space, focus it.
  if isFloat && sameSpace tr (getFocusTile wo)
     then setFocusTile tr
     else return ()
  -- Refresh the display.
  refreshSpace tr
  where
    win = ev_window e
    float _ = getDisplay >>= liftIO . flip mapWindow win
    tile tr = do
      c <- getConfig
      let
        lay = layout $ cSpace c tr
        ti  = (laTiles lay) ! tr
        ta  = laTable lay
        (px, py) = realPos ta ti
        (dw, dh) = realSpan ta ti
      debug "Tiling layout (pos, span):"
      dprint $ realPos ta ti
      dprint $ realSpan ta ti
      d <- getDisplay
      liftIO $ do
        moveResizeWindow d win (fi px) (fi py) (fi dw) (fi dh)
        mapWindow d win
    fi :: (Num a) => Qty u t x -> a
    fi = fromIntegral . unwrap

destroyWindowHandler e = do
  debug "Window destroyed:"
  dprint $ win
  -- If we were doing a mouse mode operation on this window, end it.
  wo <- getWorld
  case wMode wo of
    m@MMouse {} | mWin m == win -> mAbort m
    _                        -> return ()
  -- Remove the window from its tile
  modifyAllTileWindows $ const $ filter (/= win)
  -- Refresh the display
  refreshFocusSpace
  where
    win = ev_window e

