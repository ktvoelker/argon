
module Event where

import Declare
import State
import X11

import Data.Bits
import Foreign.Marshal.Alloc

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

eventLoop :: Config -> State -> X11 ()
eventLoop conf state = do
  ptr <- mallocBytes 96
  until (???) handler (???)
  free ptr

-- TODO
-- Add another monad, the X11State monad, which is:
--  type X11State a = StateT State X11 a
-- Add another constructor to the State type: Quit.
-- Make the event handler use this monad between the malloc and free.
--   It will loop until the state is Quit.

{-
loop :: Display -> (Display -> XEventPtr -> IO ()) -> IO ()
loop disp h = allocaXEvent
  (sequence_ . repeat . (\p -> nextEvent disp p >> h disp p))

safeHandler :: (Display -> Event -> IO ()) -> Display -> XEventPtr -> IO ()
safeHandler h disp ptr = getEvent ptr >>= h disp
-}

