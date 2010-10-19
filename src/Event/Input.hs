
module Event.Input where

import Command
import Debug
import Declare
import Event.Default
import Fields
import State
import Types
import X11

import Control.Monad
import Data.Bits
import Graphics.X11
import Graphics.X11.Xlib.Extras

eventMask :: Event -> X11State KeyMask
eventMask e =
  getConfig >>= return . (ev_state e .&.) . complement . cIgnoreMask

keyPressHandler, buttonPressHandler :: EventHandler

keyPressHandler e = do
  mask <- eventMask e
  sym  <- getDisplay >>= liftIO . \d -> keycodeToKeysym d (ev_keycode e) 0
  debug "keyPressHandler"
  if sym /= 0
     then do
       dprint mask
       dprint sym
       dwprint sym
       getKeys >>= maybe (return ()) runCommand . (lookup (mask, sym))
     else debug "Keysym 0!"

buttonPressHandler e = do
  debug "Button press handler:"
  dprint win
  debug "Raising and focusing window..."
  raiseAndFocusWindow win
  debug "Checking for mouse mode mask..."
  mask <- eventMask e
  conf <- getConfig
  let f = beginMouseMode e win mask btn
  debug "Mouse mode mask is:"
  dprint $ cFloatMask conf
  debug "Input mask is:"
  dprint mask
  if mask == cFloatMask conf
     then
       if btn == button1
          then f MMMove
          else
            if btn == button3
               then f MMResize
               else replay
     else replay
  where
    win = ev_window e
    btn = ev_button e
    replay = do
      debug "Replaying pointer event..."
      d <- getDisplay
      liftIO $ allowEvents d replayPointer $ ev_time e

beginMouseMode
  :: Event
  -> Window
  -> KeyMask
  -> Button
  -> MouseMode
  -> X11State ()
beginMouseMode e win mask btn mode = do
  wo <- getWorld
  let tr = findWindow wo win
  when (maybe False tileIsFloat tr) $ do
    disp <- getDisplay
    modifyWorld $ $(upd 'wMode) $ const MMouse
      { mMode  = mode
      , mWin   = win
      , mPosn  = posnXY (ev_x e) (ev_y e)
      , mDone  = done
      , mAbort = abort
      }
    debug "Grabbing button release and pointer motion:"
    _ <- liftIO $ do
      grabPointer
        disp win True (buttonReleaseMask .|. pointerMotionMask)
        grabModeAsync grabModeAsync none none currentTime
    return ()
  where
    abort = modifyWorld $ $(upd 'wMode) $ const MNormal
    done  = do
      disp <- getDisplay
      liftIO $ ungrabPointer disp currentTime
      abort

raiseAndFocusWindow :: Window -> X11State ()
raiseAndFocusWindow win = do
  wo <- getWorld
  let tr = findWindow wo win
  case tr of
    Nothing -> return ()
    Just tr -> do
      modifyTileWindows (flip pushFront win . filter (/= win)) tr
      setFocusTile tr
      refreshSpace tr

