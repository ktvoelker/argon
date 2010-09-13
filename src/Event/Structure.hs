
module Event.Structure where

import Action
import Attract
import Declare
import Declare.Access
import Event.Default
import Event.Listen
import Fields
import Focus
import Layout
import State
import Types
import X11

import Graphics.X11.Xlib.Extras

resizeRequestHandler, mapRequestHandler, destroyWindowHandler :: EventHandler

-- TODO for floating windows, don't ignore
resizeRequestHandler = defaultHandler

mapRequestHandler e = do
  wo <- getWorld
  -- Add standard event handlers.
  lift $ lift $ addStdEvents win
  -- Put the window where it belongs.
  tr <- attract win
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
       act $ AFocus win
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
      act $ AShow win (realPos ta ti) (realSpan ta ti)

destroyWindowHandler e = do
  -- Remove the destroyed window
  modifyAllTileWindows $ const $ filter (/= win)
  -- Focus the correct window
  updateX11Focus
  where
    win = ev_window e

