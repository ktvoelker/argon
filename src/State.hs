
module State where

import Action
import Declare
import Declare.Access
import Fields
import Types
import X11

import Control.Monad.Maybe
import Control.Monad.Reader
import Control.Monad.State

-- The MaybeT determines whether or not the user has asked the program to
-- terminate.
-- The StateT tracks the current runtime state, in the case that the user has
-- not asked the program to terminate.
type X11State a = MaybeT (StateT World (ReaderT X11Env IO)) a

getWorld :: X11State World
getWorld = lift get

putWorld :: World -> X11State ()
putWorld = lift . put

modifyWorld :: (World -> World) -> X11State ()
modifyWorld = lift . modify

quitState :: X11State a
quitState = fail "quit"

runX11State :: World -> X11State a -> X11 (Maybe a, World)
runX11State world = flip runStateT world . runMaybeT

-- Execute the actions.
runActions :: X11State ()
runActions =
  getWorld
  >>= lift . lift . mapM_ runAction . toList . wActions
  >>  modifyWorld (\w -> w { wActions = empty })

-- Enqueue an action.
act :: Action -> X11State ()
act a = modifyWorld $ $(upd 'wActions) $ insert a

data World = World
  { wSpaces  :: Map SpaceRef WSpace
  , wFocus   :: TileRef
  , wActions :: Queue Action
  } deriving (Show)

data WSpace = WSpace
  { wsFocus  :: TileRef
  , wsTiles  :: Map TileRef (Queue Window)
  , wsStatus :: Map StatusRef String
  } deriving (Show)

getSpace :: (RefSpace a) => World -> a -> WSpace
getSpace wo sr = wSpaces wo ! getSpaceRef sr

modifySpace :: (RefSpace a) => (WSpace -> WSpace) -> a -> X11State ()
modifySpace f sr =
  modifyWorld $ $(upd 'wSpaces) $ adjust f $ getSpaceRef sr

modifyFocusSpace :: (WSpace -> WSpace) -> X11State ()
modifyFocusSpace f = getWorld >>= modifySpace f . getFocusTile

getFocusTile :: World -> TileRef
getFocusTile = wFocus

setFocusTile :: TileRef -> X11State ()
setFocusTile tr = do
  modifyWorld ($(upd 'wFocus) ctr)
  modifySpace ($(upd 'wsFocus) ctr) tr
  where
    ctr = const tr

getFocusWindow :: World -> Maybe Window
getFocusWindow w = top $ getTileWindows w $ getFocusTile w

getTileWindows :: World -> TileRef -> Queue Window
getTileWindows w tr = wsTiles (getSpace w tr) ! tr

modifyTileWindows :: (Queue Window -> Queue Window) -> TileRef -> X11State ()
modifyTileWindows f tr = modifySpace ($(upd 'wsTiles) $ adjust f tr) tr

modifyFocusWindows :: (Queue Window -> Queue Window) -> X11State ()
modifyFocusWindows f = getWorld >>= modifyTileWindows f . getFocusTile

findWindow :: World -> Window -> Maybe TileRef
-- TODO
findWindow _ _ = Nothing

emptyWorld :: Config -> World
emptyWorld c = World
  { wSpaces  = fmap emptyWSpace $ cSpaces c
  , wFocus   = spStartTile $ cSpace c $ cStartSpace c
  , wActions = empty
  }

emptyWSpace :: Workspace -> WSpace
emptyWSpace w = WSpace
  { wsFocus  = spStartTile w
  , wsTiles  = emptyWLayout empty $ spLayout w
  , wsStatus = emptyWLayout "" $ stLayout $ spStatus w
  }

emptyWLayout :: a -> Layout t r -> Map r a
emptyWLayout e x = fmap (const e) $ laTiles x

