
module Action where

import Maths.Unsafe
import Types
import X11

import Graphics.X11

data Action =
    AHide  Window
  | AShow  Window (XY Posn Pix) (XY Span Pix)
  | AStack [Window]
  | AFocus Window
  deriving (Eq, Ord, Show)

runAction :: Action -> X11 ()

runAction (AHide w) = display >>= liftIO . flip mapWindow w

runAction (AShow w (px, py) (dw, dh)) = do
  d <- display
  liftIO $ moveResizeWindow d w (fi px) (fi py) (fi dw) (fi dh)
  liftIO $ mapWindow d w
  where
    fi :: (Wrapper w, Num b) => w x -> b
    fi = fromIntegral . unwrap

runAction (AStack ws) = display >>= liftIO . flip restackWindows ws

runAction (AFocus w) =
  display >>= \d -> liftIO $ setInputFocus d w revertToNone 0

