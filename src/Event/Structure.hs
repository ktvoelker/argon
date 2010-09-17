
module Event.Structure where

import Action
import Attract
import Declare
import Declare.Access
import Debug
import Event.Default
import Event.Listen
import Fields
import Layout
import State
import Types
import X11

import Graphics.X11.Xlib.Extras

resizeRequestHandler, mapRequestHandler, destroyWindowHandler :: EventHandler

-- TODO for floating windows, don't ignore
resizeRequestHandler = defaultHandler

mapRequestHandler e = do
  debug "Map request!"
  wo <- getWorld
  -- Add to the list of live windows.
  insertLiveWindow win
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
  -- Display the window.
  if isFloat
     then float tr
     else tile tr
  -- Determine if the new window should now be focused.
  let trFocus = getFocusTile wo
  -- A new window on a non-focused space never gains the focus.
  -- Otherwise:
  --   A new floating window gains the focus.
  --   A new tiled window in the focused tile gains the focus.
  if sameSpace tr trFocus && (isFloat || tr == trFocus)
     then do
       setFocusTile tr
       updateX11Focus
     else return ()
  where
    win = ev_window e
    -- TODO use the requested size of the window
    float tr = act $ AShow win (posnXY 0 0) (spanXY 100 100)
    tile tr = do
      c <- getConfig
      let
        lay = layout $ cSpace c tr
        ti  = (laTiles lay) ! tr
        ta  = laTable lay
      debug "Tiling layout (pos, span):"
      dprint $ realPos ta ti
      dprint $ realSpan ta ti
      act $ AShow win (realPos ta ti) (realSpan ta ti)

destroyWindowHandler e = do
  debug "Window destroyed:"
  dprint $ win
  -- Remove the window from the list of live windows
  deleteDeadWindow win
  -- Remove the window from its tile
  modifyAllTileWindows $ const $ filter (/= win)
  -- Update the focus
  updateX11Focus
  where
    win = ev_window e

