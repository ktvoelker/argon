
module Event.Default where

import State
import Types

import Graphics.X11
import Graphics.X11.Xlib.Extras

type EventHandler = Event -> X11State ()

defaultHandler :: EventHandler
defaultHandler _ = return ()

