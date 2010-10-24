
module Event.Structure where

import Attract
import Declare
import Declare.Access
import Debug
import Event.Default
import Event.Listen
import Fields
import Layout
import State
import Tile
import Types
import X11

import Data.List (isSuffixOf)
import Graphics.X11
import Graphics.X11.Xlib.Extras

mapRequestHandler, destroyWindowHandler, mappingNotifyHandler :: EventHandler

mapRequestHandler e = do
  debug "Map request!"
  wo <- getWorld
  -- Add standard event handlers.
  lift $ lift $ addStdEvents win
  -- Find out where the window belongs.
  tr <- attract win
  debug "Destination tile:"
  dprint tr
  -- Add the window to the tile.
  addWin tr win
  -- Check if the tile is the floating tile.
  let isFloat = tileIsFloat tr
  -- Map the window.
  debug "Map new window"
  getDisplay >>= liftIO . flip mapWindow win
  -- If the new window is floating on the focused space, focus it.
  if isFloat && sameSpace tr (getFocusTile wo)
     then setFocusTile tr
     else return ()
  -- Refresh the display.
  debug "Refresh"
  refreshSpace tr
  where
    win = ev_window e

destroyWindowHandler e = do
  debug "Window destroyed:"
  dprint $ win
  -- If we were doing a mouse mode operation on this window, end it.
  wo <- getWorld
  case wMode wo of
    m@MMouse {} | mWin m == win -> mAbort m
    _ -> return ()
  -- Is this a window we have been managing?
  --   It seems that when we kill a client, we get destroy events for
  --   child windows that we weren't aware of, which causes us to call
  --   refresh, which causes us to manipulate the main window of the
  --   client, even though it has already been destroyed, too.
  whenJust (findWindow wo win) $ const $ do
    -- Remove the window from its tile
    modifyAllTileWindows $ const $ filter (/= win)
    -- Do not refresh the display here.
    -- It isn't necessary, and if the client is in the act of closing
    -- a bunch of windows, we might tinker with one that has already
    -- been destroyed.
  where
    win = ev_window e

mappingNotifyHandler e = do
  debug "Keyboard remapped!"
  c <- getConfig
  w <- getWorld
  let km = cAllKeys c $ wKeyMode w
  lift $ lift $ do
    ungrabKeyMap km
    grabKeyMap km

