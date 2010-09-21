 
module Event.MouseMode where

import Event.Default
import Fields
import Maths.Unsafe
import State
import Types
import X11

import Graphics.X11
import Graphics.X11.Xlib.Extras

buttonReleaseHandler, motionNotifyHandler :: EventHandler

buttonReleaseHandler _ = do
  wo <- getWorld
  case wMode wo of
    MNormal     -> return () -- impossible!
    m@MMouse {} -> mDone m

motionNotifyHandler e = do
  wo <- getWorld
  case wMode wo of
    MNormal     -> return ()
    m@MMouse {} -> do
      case mMode m of
        MMMove   -> moveHandler m e
        MMResize -> resizeHandler m e

moveHandler, resizeHandler :: Mode -> Event -> X11State ()

moveHandler m e = do
  -- Calculate the difference between the current and previous mouse
  -- positions.
  let (x, y) = mPosn m
  let xy'@(x', y') = posnXY (ev_x e) (ev_y e)
  let (dx, dy) = (x' -. x, y' -. y)
  -- Record the current mouse position.
  modifyWorld $ $(upd 'wMode) $ $(upd 'mPosn) $ const xy'
  -- Get the window's current position.
  disp <- getDisplay
  (_, wx, wy, _, _, _, _) <- liftIO $ getGeometry disp win
  -- Move the window by the difference.
  let (wx', wy') = (wx + unwrap dx, wy + unwrap dy)
  liftIO $ moveWindow disp win wx' wy'
  where
    win = ev_window e

resizeHandler _ = defaultHandler

