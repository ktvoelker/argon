
module Init where

import Declare
import Event
import X11

import Graphics.X11
import Graphics.X11.Xlib.Extras

getXInfo :: X11 XInfo
getXInfo = do
  (d, s) <- dispScr
  return XInfo
    { width      = fromIntegral $ displayWidth d s
    , height     = fromIntegral $ displayHeight d s
    , fontWidth  = 0
    , fontHeight = 0
    }

initEvents :: X11 ()
initEvents = do
  root <- dispScr >>= liftIO . uncurry rootWindow
  addRootEvents root
  (_, _, wins) <- display >>= liftIO . flip queryTree root
  mapM_ addStdEvents wins

