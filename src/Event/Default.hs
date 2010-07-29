
module Event.Default where

import Declare
import State

import Graphics.X11
import Graphics.X11.Xlib.Extras

type EventHandler = Config -> Event -> X11State ()

defaultHandler :: EventHandler
defaultHandler _ _ = return ()

