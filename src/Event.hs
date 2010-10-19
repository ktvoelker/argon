
module Event (module Event, module Event.Listen) where

import Command
import Debug
import Declare
import Event.Default
import Event.Input
import Event.Listen
import Event.MouseMode
import Event.Structure
import State
import Types
import X11

import Foreign.Marshal.Alloc
import Graphics.X11
import Graphics.X11.Xlib.Extras

runTriggerImpl :: Trigger -> X11State ()
runTriggerImpl t = do
  c <- getConfig
  case lookup t $ cTriggers c of
    Nothing  -> return ()
    Just cmd -> runCommand cmd

eventLoop :: X11 ()
eventLoop = do
  debug "Get config"
  conf <- getConfig
  let world = (emptyWorld conf) { wTrigger = runTriggerImpl }
  debug "Allocate event pointer"
  ptr <- lift $ mallocBytes 96
  debug "Start looping"
  runX11State world $ do
    runTrigger TReady
    sequence_ $ repeat $ do
      -- Get the next event from the server.
      -- debug "Get next event"
      getDisplay >>= liftIO . flip nextEvent ptr
      -- Extract a safe event value from the event pointer and handle it.
      -- debug "Extract and handle event"
      safely ptr handler
  debug "Free event pointer"
  lift $ Foreign.Marshal.Alloc.free ptr

safely :: XEventPtr -> (Event -> X11State a) -> X11State a
safely ptr = ((lift . lift . lift) (getEvent ptr) >>=)

handlers :: Map Graphics.X11.EventType (Event -> X11State ())
handlers = fromList
  [ (keyPress,      keyPressHandler)
  , (buttonPress,   buttonPressHandler)
  , (mapRequest,    mapRequestHandler)
  , (destroyNotify, destroyWindowHandler)
  , (mappingNotify, mappingNotifyHandler)
  , (buttonRelease, buttonReleaseHandler)
  , (motionNotify,  motionNotifyHandler)
  ]

handler :: EventHandler
handler e = findWithDefault defaultHandler (ev_event_type e) handlers e

