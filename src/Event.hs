
module Event where

import Action
import Attract
import Declare
import State
import X11

import Data.Bits
import Foreign.Marshal.Alloc
import Graphics.X11
import Graphics.X11.Xlib.Extras
import Prelude hiding (span, filter)

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
  , (mapRequest,    mapRequestHandler)
  ]

handler, defaultHandler,
  keyReleaseHandler, buttonPressHandler,
  resizeRequestHandler, mapRequestHandler
  :: Config -> Event -> X11State [Action]

handler c e = findWithDefault defaultHandler (ev_event_type e) handlers c e

defaultHandler _ _ = quitState

keyReleaseHandler = defaultHandler

buttonPressHandler = defaultHandler

-- TODO ignore except for floating windows
resizeRequestHandler = defaultHandler

-- TODO add the window to the appropriate window collection in the World.
-- TODO if the window wasn't attracted away from the focus, tell X to focus it
--   (This requires implementing a new Action constructor, AFocus.)
-- TODO if the window is floating and focused, record the new focus.
mapRequestHandler c e = do
  wo <- getWorld
  (wSpaceName, wTileName) <- attract win
  a <- case wTileName of
    Nothing        -> float wSpaceName
    Just wTileName -> return $ tile wSpaceName wTileName
  return [a]
  where
    win = ev_window e
    -- TODO use the requested size of the window
    float sn = return $ AShow win (0, 0) (100, 100)
    -- TODO use the position and size of the tile
    tile sn tn = AShow win (100, 100) (100, 100)

