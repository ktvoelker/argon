
module Event where

import Debug
import Declare
import Event.Default
import Event.Input
import Event.Structure
import State
import Types
import X11

import Data.Bits
import Foreign.Marshal.Alloc
import Graphics.X11
import Graphics.X11.Xlib.Extras

addStdEvents :: Window -> X11 ()
addStdEvents win = do
  disp <- getDisplay
  liftIO $ do
    grabButton
      disp anyButton anyModifier win True
      buttonPressMask grabModeSync grabModeAsync none none
    selectInput disp win (keyPressMask .|. keyReleaseMask)

addRootEvents :: Window -> X11 ()
addRootEvents win = do
  disp <- getDisplay
  debug "Root is:"
  dprint win
  debug "Select resize redir on root"
  liftIO $ selectInput disp win resizeRedirectMask
  debug "Select substruct redir on root"
  liftIO $ selectInput disp win substructureRedirectMask

eventLoop :: X11 ()
eventLoop = do
  debug "Get config"
  conf <- getConfig
  let world = emptyWorld conf
  debug "Allocate event pointer"
  ptr <- lift $ mallocBytes 96
  debug "Start looping"
  runX11State world $ sequence_ $ repeat $ do
    -- Get the next event from the server.
    debug "Get next event"
    getDisplay >>= liftIO . flip nextEvent ptr
    -- Extract a safe event value from the event pointer and handle it.
    debug "Extract and handle event"
    safely ptr (handler conf)
    -- Run the actions emitted by the handler.
    debug "Run actions"
    runActions
  debug "Free event pointer"
  lift $ Foreign.Marshal.Alloc.free ptr

safely :: XEventPtr -> (Event -> X11State a) -> X11State a
safely ptr = ((lift . lift . lift) (getEvent ptr) >>=)

handlers :: Map Graphics.X11.EventType (Config -> Event -> X11State ())
handlers = fromList
  [ (keyRelease,    keyReleaseHandler)
  , (buttonPress,   buttonPressHandler)
  , (resizeRequest, resizeRequestHandler)
  , (mapRequest,    mapRequestHandler)
  ]

handler :: EventHandler
handler c e = findWithDefault defaultHandler (ev_event_type e) handlers c e

