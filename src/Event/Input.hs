
module Event.Input where

import Command
import Debug
import Declare
import Event.Default
import State
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
  if sym /= 0
     then do
       dprint mask
       dprint sym
       dwprint sym
       getConfig
         >>= maybe (return ()) runCommand . (lookup (mask, sym)) . cKeys
     else debug "Keysym 0!"

buttonPressHandler e = do
  debug "Button press handler:"
  dprint $ ev_window e
  raiseAndFocusWindow $ ev_window e
  d <- getDisplay
  liftIO $ allowEvents d replayPointer $ ev_time e

