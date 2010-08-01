
module Event.Input where

import Command
import Debug
import Declare
import Event.Default
import Types
import X11

import Data.Bits
import Graphics.X11
import Graphics.X11.Xlib.Extras

keyReleaseHandler, buttonPressHandler :: EventHandler

keyReleaseHandler e = do
  mask <- getConfig >>= return . (ev_state e .&.) . complement . cIgnoreMask
  sym  <- getDisplay
    >>= lift . lift . lift . \d -> keycodeToKeysym d (ev_keycode e) 0
  debug "keyReleaseHandler"
  dprint mask
  dwprint sym
  getConfig >>= maybe (return ()) runCommand . (lookup (mask, sym)) . cKeys

buttonPressHandler = defaultHandler

