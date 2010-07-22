
module Action where

import Graphics.X11

import Types
import X11

data Action =
    AHide  Window
  | AShow  Window (Nat, Nat) (Nat, Nat)
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
    fi :: (Integral a, Num b) => a -> b
    fi = fromIntegral

runAction (AStack ws) = display >>= liftIO . flip restackWindows ws

runAction (AFocus w) =
  display >>= \d -> liftIO $ setInputFocus d w revertToNone 0

