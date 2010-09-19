
module Event.Input where

import Command
import Debug
import Declare
import Event.Default
import Fields
import History.Tile
import State
import Types
import X11

import Data.Bits
import Graphics.X11
import Graphics.X11.Xlib.Extras

eventMask :: Event -> X11State KeyMask
eventMask e =
  getConfig >>= return . (ev_state e .&.) . complement . cIgnoreMask

keyReleaseHandler, buttonPressHandler :: EventHandler

keyReleaseHandler e = do
  mask <- eventMask e
  sym  <- getDisplay >>= liftIO . \d -> keycodeToKeysym d (ev_keycode e) 0
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
  dprint win
  mask <- eventMask e
  conf <- getConfig
  let f = beginMouseMode win mask btn
  if mask == cFloatMask conf
     then
       if btn == button1
          then f MMMove
          else
            if btn == button2
               then f MMResize
               else raiseAndFocusWindow e
     else raiseAndFocusWindow e
  where
    win = ev_window e
    btn = ev_button e

beginMouseMode
  :: Window
  -> KeyMask
  -> Button
  -> MouseMode
  -> X11State ()
beginMouseMode win mask btn mode = do
  -- TODO check if this is a root window, and ignore
  disp <- getDisplay
  modifyWorld $ $(upd 'wMode) $ const MMouse
    { mMode  = mode
    , mWin   = win
    , mPosn  = undefined -- TODO
    , mDone  = done
    , mAbort = abort
    }
  liftIO $ do
    grabButton
      disp btn mask win True
      buttonReleaseMask grabModeSync grabModeAsync none none
    selectInput disp win pointerMotionMask
  where
    abort = modifyWorld $ $(upd 'wMode) $ const MNormal
    done  = do
      disp <- getDisplay
      liftIO $ do
        selectInput disp win 0
        ungrabButton disp btn mask win
      abort

raiseAndFocusWindow :: Event -> X11State ()
raiseAndFocusWindow e = do
  wo <- getWorld
  let tr = findWindow wo win
  case tr of
    Nothing -> return ()
    Just tr -> do
      modifyTileWindows (flip pushFront win . filter (/= win)) tr
      setFocusTile tr
      refreshSpace tr
  d <- getDisplay
  liftIO $ allowEvents d replayPointer $ ev_time e
  where
    win = ev_window e

