
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
  debug "Add root events"
  addRootEvents
  debug "Query window tree"
  root <- getRoot
  dprint root
  (_, _, wins) <- getDisplay >>= liftIO . flip queryTree root
  mapM_ addStdEvents wins

