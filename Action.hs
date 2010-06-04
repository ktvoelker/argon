
module Action where

import Graphics.X11

import Types

data Action =
    AHide  Window
  | AShow  Window (Nat, Nat) (Nat, Nat)
  | AStack [Window]
  deriving (Eq, Ord, Show)

-- TODO
act :: Action -> IO ()
act _ = return ()

