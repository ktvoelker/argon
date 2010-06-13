
module Event where

import Action
import Declare
import State
import X11

import Data.Bits
import Foreign.Marshal.Alloc
import Graphics.X11
import Graphics.X11.Xlib.Extras

addStdEvents :: Window -> X11 ()
addStdEvents win = do
  disp <- display
  liftIO $ grabButton
    disp anyButton anyModifier win True
    buttonPressMask grabModeSync grabModeAsync none none

addRootEvents :: Window -> X11 ()
addRootEvents win = do
  disp <- display
  liftIO $ selectInput disp win
    (resizeRedirectMask .|. substructureRedirectMask .|. substructureNotifyMask)

eventLoop :: Config -> World -> X11 ()
eventLoop conf world = do
  ptr <- lift $ mallocBytes 96
  runX11State world $
    sequence_ $ map actX11State $ repeat $ safe ptr $ handler conf
  lift $ free ptr

safe :: XEventPtr -> (Event -> X11State a) -> X11State a
safe ptr = ((lift . lift . lift) (getEvent ptr) >>=)

-- TODO
handler :: Config -> Event -> X11State [Action]
handler _ _ = quitState

