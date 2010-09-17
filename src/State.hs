
module State where

import Action
import Debug
import Declare
import Declare.Access
import Fields
import Types
import X11

import Control.Monad.Maybe
import Control.Monad.Reader
import Control.Monad.State
import Data.Maybe
import Graphics.X11
import Graphics.X11.Xlib.Extras

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
  { wFocus   :: TileRef
  , wActions :: Queue Action
  , wFocuses :: Map SpaceRef TileRef
  , wTiles   :: Map TileRef (BankersDequeue Window)
  , wStatus  :: Map StatusRef String
  , wWindows :: Set Window
  } deriving (Show)

getLocalFocus :: (RefSpace a) => World -> a -> TileRef
getLocalFocus w sr = wFocuses w ! getSpaceRef sr

modifyLocalFocus :: (RefSpace a) => (TileRef -> TileRef) -> a -> X11State ()
modifyLocalFocus f sr =
  modifyWorld $ $(upd 'wFocuses) $ adjust f $ getSpaceRef sr

getFocusTileM :: X11State TileRef
getFocusTileM = getWorld >>= return . getFocusTile

getFocusTile :: World -> TileRef
getFocusTile = wFocus

setFocusTile :: TileRef -> X11State ()
setFocusTile tr = do
  modifyWorld ($(upd 'wFocus) ctr)
  modifyLocalFocus ctr tr
  where
    ctr = const tr

getFocusWindow :: X11State Window
getFocusWindow = do
  d <- lift $ lift $ getDisplay
  w <- getWorld
  return
    $ fromMaybe (defaultRootWindow d)
    $ first
    $ getTileWindows w
    $ getFocusTile w

updateX11Focus :: X11State ()
updateX11Focus = do
  win <- getFocusWindow
  debug "Update X11 focus:"
  dprint win
  getDisplay >>= dprint . defaultRootWindow
  act $ AFocus win

getTileWindows :: World -> TileRef -> BankersDequeue Window
getTileWindows w = (wTiles w !)

modifyTileWindows
  :: (BankersDequeue Window -> BankersDequeue Window)
  -> TileRef
  -> X11State ()
modifyTileWindows f tr = do
  d  <- getDisplay
  wo <- getWorld
  let tiles  = wTiles wo ! tr
  let tiles' = f tiles
  put $ $(upd 'wTiles) (insert (tr, tiles')) wo
  if tileIsFloat tr
     then liftIO $ restackWindows d $ toList tiles'
     else do
       let hidden = first tiles
       let shown  = first tiles'
       if hidden == shown
          then return ()
          else do
            let j = justIfLive wo
            debug "Unmapping hidden window:"
            dprint hidden
            dprint (hidden >>= j)
            maybe (return ()) (liftIO . unmapWindow d) $ (hidden >>= j)
            debug "Mapping exposed window:"
            dprint shown
            dprint (shown >>= j)
            maybe (return ()) (liftIO . mapWindow d) $ (shown >>= j)

justIfLive :: World -> Window -> Maybe Window
justIfLive wo win = if member win $ wWindows wo then Just win else Nothing

modifyAllTileWindows
  :: (TileRef -> BankersDequeue Window -> BankersDequeue Window)
  -> X11State ()
modifyAllTileWindows f =
  getWorld >>= mapM_ (\k -> modifyTileWindows (f k) k) . keys . wTiles

modifyFocusWindows
  :: (BankersDequeue Window -> BankersDequeue Window) -> X11State ()
modifyFocusWindows f = getWorld >>= modifyTileWindows f . getFocusTile

insertLiveWindow :: Window -> X11State ()
insertLiveWindow = modifyWorld . $(upd 'wWindows) . insert

deleteDeadWindow :: Window -> X11State ()
deleteDeadWindow = modifyWorld . $(upd 'wWindows) . delete

emptyWorld :: Config -> World
emptyWorld c = World
  { wFocuses = fmap spStartTile $ cSpaces c
  , wFocus   = spStartTile $ cSpace c $ cStartSpace c
  , wActions = empty
  , wTiles   = emptyWLayout empty spLayout spacesList
  , wStatus  = emptyWLayout "" (stLayout . spStatus) spacesList
  , wWindows = empty
  }
  where
    spacesList = elems $ cSpaces c

-- Create an empty state for some things which have layouts.
emptyWLayout
  :: (Ord r)
  => a                  -- Empty value to associate with each tile.
  -> (b -> Layout t r)  -- Accessor to retrieve layout from each thing.
  -> [b]                -- Some things which have layouts.
  -> Map r a            -- A map from each tile to the empty value.
emptyWLayout e f = unions . map (fmap (const e) . laTiles . f)

