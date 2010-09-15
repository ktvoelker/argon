
module Event.Listen where

import Debug
import Declare
import Types
import X11

import Data.Bits
import Graphics.X11
import Graphics.X11.Xlib.Extras

addStdEvents :: Window -> X11 ()
addStdEvents win = do
  debug "Add standard events"
  dprint win
  disp <- getDisplay
  liftIO $ do
    grabButton
      disp anyButton anyModifier win True
      buttonPressMask grabModeSync grabModeAsync none none

addRootEvents :: Window -> X11 ()
addRootEvents win = do
  disp <- getDisplay
  debug "Root is:"
  dprint win
  debug "Select resize redir on root"
  liftIO $ selectInput disp win resizeRedirectMask
  debug "Select substruct redir on root"
  liftIO $ selectInput disp win substructureRedirectMask
  c <- getConfig
  liftIO $ mapM_ (uncurry $ g disp) $ keys $ cKeys c
  where
    g disp mod sym = do
      code <- keysymToKeycode disp sym
      grabKey
        disp code mod win
        False grabModeAsync grabModeAsync

