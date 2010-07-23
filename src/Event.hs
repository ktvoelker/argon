
module Event where

import Action
import Attract
import Debug
import Declare
import Fields
import State
import X11

import Data.Bits
import Foreign.Marshal.Alloc
import Graphics.X11
import Graphics.X11.Xlib.Extras
import Prelude hiding (span, filter)

addStdEvents :: Window -> X11 ()
addStdEvents win = do
  disp <- display
  liftIO $ do
    grabButton
      disp anyButton anyModifier win True
      buttonPressMask grabModeSync grabModeAsync none none
    selectInput disp win (keyPressMask .|. keyReleaseMask)

addRootEvents :: Window -> X11 ()
addRootEvents win = do
  disp <- display
  debug "Root is:"
  dprint win
  debug "Select resize redir on root"
  liftIO $ selectInput disp win resizeRedirectMask
  debug "Select substruct redir on root"
  liftIO $ selectInput disp win substructureRedirectMask

eventLoop :: X11 ()
eventLoop = do
  debug "Get config"
  conf <- config
  let world = emptyWorld conf
  debug "Allocate event pointer"
  ptr <- lift $ mallocBytes 96
  debug "Start looping"
  runX11State world $ sequence_ $ repeat $ do
    -- Get the next event from the server.
    debug "Get next event"
    display >>= liftIO . flip nextEvent ptr
    -- Extract a safe event value from the event pointer and handle it.
    debug "Extract and handle event"
    safely ptr (handler conf)
    -- Run the actions emitted by the handler.
    debug "Run actions"
    runActions
  debug "Free event pointer"
  lift $ free ptr

safely :: XEventPtr -> (Event -> X11State a) -> X11State a
safely ptr = ((lift . lift . lift) (getEvent ptr) >>=)

handlers :: Map Graphics.X11.EventType (Config -> Event -> X11State ())
handlers = fromList
  [ (keyRelease,    keyReleaseHandler)
  , (buttonPress,   buttonPressHandler)
  , (resizeRequest, resizeRequestHandler)
  , (mapRequest,    mapRequestHandler)
  ]

handler, defaultHandler,
  keyReleaseHandler, buttonPressHandler,
  resizeRequestHandler, mapRequestHandler
  :: Config -> Event -> X11State ()

handler c e = findWithDefault defaultHandler (ev_event_type e) handlers c e

defaultHandler _ _ = return ()

keyReleaseHandler = defaultHandler

buttonPressHandler = defaultHandler

-- TODO for floating windows, don't ignore
resizeRequestHandler = defaultHandler

mapRequestHandler c e = do
  wo <- getWorld
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
      act $ tile wSpaceName wTileName
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
    float sn = act $ AShow win (0, 0) (100, 100)
    tile sn tn = AShow win (px, py) (sx, sy)
      where
        lay = layout $ spaces c ! sn
        Tile { pos = (pc, pr), span = (sc, sr) } = (tiles lay) ! tn
        Table { rows = rs, cols = cs } = table lay
        (bcs, acs) = splitAt pc cs
        (brs, ars) = splitAt pr rs
        px = sum bcs
        py = sum brs
        sx = head acs
        sy = head ars

