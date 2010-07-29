
module Event.Input where

import Command
import Declare
import Event.Default
import Types
import X11

import Data.Maybe
import Graphics.X11
import Graphics.X11.Xlib.Extras

keyReleaseHandler, buttonPressHandler :: EventHandler

keyReleaseHandler e = do
  let mod = ev_state e
  sym <- getDisplay
    >>= lift . lift . lift . \d -> keycodeToKeysym d (ev_keycode e) 0
  getConfig >>=
    fromMaybe (return ()) . fmap runCommand . (lookup (mod, sym)) . cKeys

buttonPressHandler = defaultHandler

