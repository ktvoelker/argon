
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
  liftIO $ do
    grabButton
      disp anyButton anyModifier win True
      buttonPressMask grabModeSync grabModeAsync none none
    selectInput disp win (keyPressMask .|. keyReleaseMask)

addRootEvents :: Window -> X11 ()
addRootEvents win = do
  disp <- display
  liftIO $ selectInput disp win
    (resizeRedirectMask .|.
     substructureRedirectMask .|. substructureNotifyMask)

eventLoop :: Config -> World -> X11 ()
eventLoop conf world = do
  ptr <- lift $ mallocBytes 96
  runX11State world $
    sequence_ $ map actX11State $ repeat $ safe ptr $ handler conf
  lift $ free ptr

safe :: XEventPtr -> (Event -> X11State a) -> X11State a
safe ptr = ((lift . lift . lift) (getEvent ptr) >>=)

handlers :: Map Graphics.X11.EventType (Config -> Event -> X11State [Action])
handlers = fromList
  [ (keyRelease,    keyReleaseHandler)
  , (buttonPress,   buttonPressHandler)
  , (resizeRequest, resizeRequestHandler)
  , (mapNotify,     mapNotifyHandler)
  , (mapRequest,    mapRequestHandler)
  ]

handler, defaultHandler, keyReleaseHandler, buttonPressHandler,
  resizeRequestHandler, mapNotifyHandler, mapRequestHandler
  :: Config -> Event -> X11State [Action]

handler c e = findWithDefault defaultHandler (ev_event_type e) handlers c e

defaultHandler _ _ = quitState

keyReleaseHandler = defaultHandler

buttonPressHandler = defaultHandler

resizeRequestHandler = defaultHandler

mapNotifyHandler = defaultHandler

mapRequestHandler = defaultHandler

