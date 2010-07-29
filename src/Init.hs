
module Init where

import Debug
import Declare
import Event
import X11

import Graphics.X11
import Graphics.X11.Xlib.Extras

getXInfo :: X11 XInfo
getXInfo = do
  (d, s) <- dispScr
  return XInfo
    { width      = span $ fromIntegral $ displayWidth d s
    , height     = span $ fromIntegral $ displayHeight d s
    , fontWidth  = span 0  -- TODO
    , fontHeight = span 0  -- TODO
    }

initEvents :: X11 ()
initEvents = do
  debug "Get root window"
  root <- dispScr >>= liftIO . uncurry rootWindow
  debug "Add root events"
  addRootEvents root
  debug "Query window tree"
  dprint root
  (_, _, wins) <- getDisplay >>= liftIO . flip queryTree root
  debug "Add standard events"
  mapM_ addStdEvents wins

