
module Action where

import Graphics.X11

import Types
import X11

data Action =
    AHide  Window
  | AShow  Window (Nat, Nat) (Nat, Nat)
  | AStack [Window]
  deriving (Eq, Ord, Show)

act :: Action -> X11 ()

act (AHide w) = display >>= lift . flip mapWindow w

act (AShow w (px, py) (dw, dh)) = do
  d <- display
  liftIO $ moveResizeWindow d w (fi px) (fi py) (fi dw) (fi dh)
  liftIO $ mapWindow d w
  where
    fi :: (Integral a, Num b) => a -> b
    fi = fromIntegral

act (AStack ws) = display >>= lift . flip restackWindows ws

