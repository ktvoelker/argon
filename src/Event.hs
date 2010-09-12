
module Event (module Event, module Event.Listen) where

import Debug
import Declare
import Event.Default
import Event.Input
import Event.Listen
import Event.Structure
import State
import Types
import X11

import Foreign.Marshal.Alloc
import Graphics.X11
import Graphics.X11.Xlib.Extras

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
    safely ptr handler
    -- Run the actions emitted by the handler.
    debug "Run actions"
    runActions
  debug "Free event pointer"
  lift $ Foreign.Marshal.Alloc.free ptr

safely :: XEventPtr -> (Event -> X11State a) -> X11State a
safely ptr = ((lift . lift . lift) (getEvent ptr) >>=)

handlers :: Map Graphics.X11.EventType (Event -> X11State ())
handlers = fromList
  [ (keyRelease,    keyReleaseHandler)
  , (buttonPress,   buttonPressHandler)
  , (resizeRequest, resizeRequestHandler)
  , (mapRequest,    mapRequestHandler)
  , (destroyNotify, destroyWindowHandler)
  ]

handler :: EventHandler
handler e = findWithDefault defaultHandler (ev_event_type e) handlers e

