
module Action where

import Graphics.X11

import Types
import X11

data Action =
    AHide  Window
  | AShow  Window (Nat, Nat) (Nat, Nat)
  | AStack [Window]
  deriving (Eq, Ord, Show)

-- TODO
act :: Action -> X11 ()
act _ = return ()

