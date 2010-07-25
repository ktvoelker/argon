
module State where

import Action
import Declare
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
  { wSpaces  :: Map Name WSpace
  , wFocus   :: Name
  , wActions :: Queue Action
  } deriving (Show)

data WSpace = WSpace
  { wsFocus  :: Either (Maybe Window) Name
  , wsTiles  :: Map Name (Queue Window)
  , wsFloats :: Stack Window
  , wsStatus :: Map Name String
  } deriving (Show)

wFocusSpace :: World -> WSpace
wFocusSpace w = wSpaces w ! wFocus w

modifySpace :: Name -> (WSpace -> WSpace) -> X11State ()
modifySpace n f =
  modifyWorld (\w ->
    $(upd 'wSpaces) (insert (n, f $ wSpaces w ! n)) w)

modifyFocusSpace :: (WSpace -> WSpace) -> X11State ()
modifyFocusSpace f = getWorld >>= flip modifySpace f . wFocus

wholeFocus :: World -> (Name, Either (Maybe Window) Name)
wholeFocus w = (wFocus w, wsFocus $ wSpaces w ! wFocus w)

emptyWorld :: Config -> World
emptyWorld c = World
  { wSpaces  = fmap emptyWSpace $ cSpaces c
  , wFocus   = cStartSpace c
  , wActions = empty
  }

emptyWSpace :: Workspace -> WSpace
emptyWSpace w = WSpace
  { wsFocus  = maybe (Left Nothing) Right $ spStartTile w
  , wsTiles  = emptyWLayout empty $ spLayout w
  , wsFloats = empty
  , wsStatus = emptyWLayout "" $ stLayout $ spStatus w
  }

emptyWLayout :: a -> Layout t -> Map Name a
emptyWLayout e x = fmap (const e) $ laTiles x

