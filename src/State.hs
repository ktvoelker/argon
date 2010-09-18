
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

findWindow :: World -> Window -> Maybe TileRef
findWindow wo win = listToMaybe $ keys $ filter (member win . snd) $ wTiles wo

raiseAndFocusWindow :: Window -> X11State ()
raiseAndFocusWindow win = do
  wo <- getWorld
  let tr = findWindow wo win
  case tr of
    Nothing -> return ()
    Just tr -> do
      modifyTileWindows (flip pushFront win . filter (/= win)) tr
      setFocusTile tr
      refreshSpace tr

getTileWindows :: World -> TileRef -> BankersDequeue Window
getTileWindows w = (wTiles w !)

-- Return all the windows of a space, partitioned into visible and invisible
-- windows, with visible windows listed from top to bottom.
partitionSpace :: (RefSpace a) => a -> X11State ([Window], [Window])
partitionSpace sr = do
  wo <- getWorld
  let
  { (tilesV, tilesH) = unzip
    $ elems
    $ fmap popFront
    $ filterWithKey (\tr _ -> not (tileIsFloat tr) && sameSpace sr tr)
    $ wTiles wo
  ; floats = toList $ getTileWindows wo $ getFloatRef sr
  ; tilesV' = catMaybes tilesV
  ; tilesH' = concatMap toList tilesH
  }
  return (floats ++ tilesV', tilesH')

spaceWindows :: (RefSpace a) => a -> X11State [Window]
spaceWindows sr = do
  wo <- getWorld
  return
    $ concatMap toList
    $ elems
    $ filterWithKey (\tr _ -> sameSpace sr tr)
    $ wTiles wo

refreshSpace :: (RefSpace a) => a -> X11State ()
refreshSpace sr = do
  wo <- getWorld
  d  <- getDisplay
  if sameSpace sr $ wFocus wo
     then do
       (v, h) <- partitionSpace sr
       liftIO $ do
         restackWindows d v
         mapM_ (unmapWindow d) h
         mapM_ (mapWindow d) v
       updateX11Focus
     else spaceWindows sr >>= liftIO . mapM_ (unmapWindow d)

refreshFocusSpace :: X11State ()
refreshFocusSpace = getFocusTileM >>= refreshSpace

modifyTileWindows
  :: (BankersDequeue Window -> BankersDequeue Window)
  -> TileRef
  -> X11State ()
modifyTileWindows f = modifyWorld . $(upd 'wTiles) . adjust f

modifyAllTileWindows
  :: (TileRef -> BankersDequeue Window -> BankersDequeue Window)
  -> X11State ()
modifyAllTileWindows = modifyWorld . $(upd 'wTiles) . mapWithKey

modifyFocusWindows
  :: (BankersDequeue Window -> BankersDequeue Window) -> X11State ()
modifyFocusWindows f = getWorld >>= modifyTileWindows f . getFocusTile

emptyWorld :: Config -> World
emptyWorld c = World
  { wFocuses = fmap spStartTile $ cSpaces c
  , wFocus   = spStartTile $ cSpace c $ cStartSpace c
  , wActions = empty
  , wTiles   = union floats $ emptyWLayout empty spLayout spacesList
  , wStatus  = emptyWLayout "" (stLayout . spStatus) spacesList
  }
  where
    spacesList = elems $ cSpaces c
    floats     = mapKeys getFloatRef $ fmap (const empty) $ cSpaces c

-- Create an empty state for some things which have layouts.
emptyWLayout
  :: (Ord r)
  => a                  -- Empty value to associate with each tile.
  -> (b -> Layout t r)  -- Accessor to retrieve layout from each thing.
  -> [b]                -- Some things which have layouts.
  -> Map r a            -- A map from each tile to the empty value.
emptyWLayout e f = unions . map (fmap (const e) . laTiles . f)

