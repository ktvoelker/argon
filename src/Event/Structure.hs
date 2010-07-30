
module Event.Structure where

import Action
import Attract
import Declare
import Event.Default
import Event.Listen
import Fields
import Layout
import State
import Types
import X11

import Graphics.X11.Xlib.Extras

resizeRequestHandler, mapRequestHandler :: EventHandler

-- TODO for floating windows, don't ignore
resizeRequestHandler = defaultHandler

mapRequestHandler e = do
  wo <- getWorld
  -- Add standard event handlers.
  lift $ lift $ addStdEvents win
  -- Put the window where it belongs.
  (wSpaceName, wTileName) <- attract win
  case wTileName of
    -- The new window is floating.
    Nothing        -> do
      -- Add the window to the top of the floating stack.
      modifySpace wSpaceName $ $(upd 'wsFloats) $ insert win
      -- Display the new window.
      float wSpaceName
    -- The new window is tiled.
    Just wTileName -> do
      -- Add the window to the front of the tile queue.
      modifySpace wSpaceName $ $(upd 'wsTiles) $ adjust (insert win) wTileName
      -- Display the new window.
      tile wSpaceName wTileName
  -- Determine if the new window should now be focused.
  let (focusSpace, focusTile) = wholeFocus wo
  if focusSpace == wSpaceName
     then case (wTileName, focusTile) of
       -- A new floating window always gains the focus.
       (Nothing, _) -> do
         -- Record that the new window has the focus.
         modifyFocusSpace (\s -> s { wsFocus = Left $ Just win })
         -- Focus the new window.
         act $ AFocus win
       -- A new tiled window in the focused tile gains the focus.
       (Just tn, Right ftn) | tn == ftn -> act $ AFocus win
       -- Otherwise, leave the focus alone.
       _ -> return ()
     else return ()
  where
    win = ev_window e
    -- TODO use the requested size of the window
    float sn = act $ AShow win (posnXY 0 0) (spanXY 100 100)
    tile sn tn = do
      c <- getConfig
      let
        lay = layout $ cSpaces c ! sn
        ti  = (laTiles lay) ! tn
        ta  = laTable lay
      act $ AShow win (realPos ta ti) (realSpan ta ti)

