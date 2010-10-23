
module Event.Listen where

import Debug
import Declare
import Declare.Access
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

addRootEvents :: X11 ()
addRootEvents = do
  disp <- getDisplay
  root <- getRoot
  debug "Root is:"
  dprint root
  debug "Select inputs on root"
  liftIO $ selectInput disp root 
    (substructureRedirectMask .|. substructureNotifyMask)
  c <- getConfig
  debug "Grab command keys on root"
  grabKeyMap $ cStartKeys c

grabKeyMap :: KeyMap -> X11 ()
grabKeyMap =
  grabUngrabKeyMap
    (\d m c w -> grabKey d m c w False grabModeAsync grabModeAsync)

ungrabKeyMap :: KeyMap -> X11 ()
ungrabKeyMap _ = getDisplay >>= liftIO . flip ungrabKeyboard currentTime
  -- grabUngrabKeyMap ungrabKey

grabUngrabKeyMap
  :: (Display -> KeyCode -> KeyMask -> Window -> IO ())
  -> KeyMap
  -> X11 ()
grabUngrabKeyMap f km = do
  disp <- getDisplay
  root <- getRoot
  mapM_ (uncurry $ g disp root) $ keys km
  where
    g disp root mod sym = do
      code <- liftIO $ keysymToKeycode disp sym
      {-
      debug' "(un)grab:"
      dprint' mod
      debug' " "
      dprint' code
      debug' " "
      dprint sym
      -}
      liftIO $ f disp code mod root

