
module Action where

import Debug
import Maths.Unsafe
import Types
import X11

import Graphics.X11

data Action =
    AHide  Window
  | AShow  Window (XYPosn Pix) (XYSpan Pix)
  | AStack [Window]
  | AFocus Window
  deriving (Eq, Ord, Show)

runAction :: Action -> X11 ()

runAction (AHide w) = getDisplay >>= liftIO . flip mapWindow w

runAction (AShow w (px, py) (dw, dh)) = do
  d <- getDisplay
  liftIO $ moveResizeWindow d w (fi px) (fi py) (fi dw) (fi dh)
  liftIO $ mapWindow d w
  where
    fi :: (Num a) => Qty u t x -> a
    fi = fromIntegral . unwrap

runAction (AStack ws) = getDisplay >>= liftIO . flip restackWindows ws

runAction (AFocus w) = do
  debug "In runAction AFocus:"
  dprint w
  getDisplay >>= \d -> liftIO $ setInputFocus d w revertToPointerRoot 0

