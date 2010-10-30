
module State where

import Debug
import Declare
import Declare.Access
import Fields
import History
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

withModifyWorld :: (World -> (a, World)) -> X11State a
withModifyWorld f = do
  wo <- getWorld
  let (a, wo') = f wo
  putWorld wo'
  return a

quitState :: X11State a
quitState = fail "quit"

runX11State :: World -> X11State a -> X11 (Maybe a, World)
runX11State world = flip runStateT world . runMaybeT

data World = World
  { wFocus   :: TileRef
  , wFocuses :: Map SpaceRef TileRef
  , wTiles   :: Map TileRef (DQ Window)
  , wHistory :: History TileRef
  , wMode    :: Mode
  , wKeyMode :: Set ModeRef
  , wFloats  :: Map SpaceRef Bool
  , wTrigger :: Trigger -> X11State ()
  } deriving (Show)

instance Show (Trigger -> X11State ()) where
  showsPrec _ _ = ("<runTrigger>" ++)

data Mode =
    MNormal
  | MMouse
    { mMode  :: MouseMode
    , mWin   :: Window
    , mPosn  :: XYPosn Pix
    , mDone  :: X11State ()
    , mAbort :: X11State ()
    } deriving (Show)

instance Show (X11State a) where
  show _ = "<X11State>"

data MouseMode = MMMove | MMResize deriving (Eq, Ord, Show)

getKeys :: X11State KeyMap
getKeys = do
  wo <- getWorld
  c  <- getConfig
  return $ cAllKeys c $ wKeyMode wo

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
  old <- getFocusTileM
  when (old /= tr) $ do
    when (tileIsFloat tr) $ setShowFloatOn True tr
    modifyWorld $ $(upd 'wFocus) ctr
    modifyLocalFocus ctr tr
    if sameSpace old tr
       then updateX11Focus
       else do
         refreshMapping old
         refreshMapping tr
         runTrigger $ TSpace $ getSpaceRef tr
    runTrigger $ TFocus tr
  where
    ctr = const tr

runTrigger :: Trigger -> X11State ()
runTrigger t = getWorld >>= flip wTrigger t

getFocusWindow :: X11State Window
getFocusWindow = do
  d <- lift $ lift $ getDisplay
  getNonRootFocusWindow >>= return . fromMaybe (defaultRootWindow d)

getNonRootFocusWindow :: X11State (Maybe Window)
getNonRootFocusWindow = do
  w <- getWorld
  return
    $ first
    $ getTileWindows w
    $ getFocusTile w

updateX11FocusCarefully :: X11State ()
updateX11FocusCarefully = do
  ok <- getFocusWindow >>= windowExists
  when ok updateX11Focus

windowExists :: Window -> X11State Bool
windowExists win = do
  disp <- getDisplay
  root <- getRoot
  (_, _, wins) <- liftIO $ queryTree disp root
  return $ win `elem` wins

updateX11Focus :: X11State ()
updateX11Focus = do
  win <- getFocusWindow
  debug "Update X11 focus:"
  dprint win
  disp <- getDisplay
  dprint $ defaultRootWindow disp
  liftIO $ setInputFocus disp win revertToPointerRoot 0

setShowFloatOn :: (RefSpace a) => Bool -> a -> X11State ()
setShowFloatOn v' rs = do
  let sr = getSpaceRef rs
  -- Is the float visible?
  v <- gets ((! sr) . wFloats)
  -- Is the visibility changing?
  when (v /= v') $ do
    -- Make sure the float is not focused when hidden.
    tr <- getFocusTileM
    when (not v' && sameSpace sr tr && tileIsFloat tr) $ do
      -- Pick a non-floating tile to focus.
      getWorld >>=
        setFocusTile
        . head
        . filter (not . tileIsFloat)
        . filter (sameSpace sr)
        . keys
        . wTiles
    -- Record the new state.
    modifyWorld $ $(upd 'wFloats) $ insert (sr, v')
    -- Refresh.
    when (sameSpace sr tr) refreshFocusMapping
    -- Fire triggers.
    runTrigger $ TShowFloat sr v

setShowFloat :: Bool -> X11State ()
setShowFloat v = getFocusTileM >>= setShowFloatOn v

findWindow :: World -> Window -> Maybe TileRef
findWindow wo win = listToMaybe $ keys $ filter (member win . snd) $ wTiles wo

getTileWindows :: World -> TileRef -> DQ Window
getTileWindows w = (wTiles w !)

-- Return all the windows of a space, partitioned into visible and invisible
-- windows, with visible windows listed from top to bottom.
partitionSpace :: (RefSpace a) => a -> X11State ([Window], [Window])
partitionSpace sr = do
  c  <- getConfig
  wo <- getWorld
  let
  { floats = toList $ getTileWindows wo $ getFloatRef sr
  ; tileds = concatMap (toList . getTileWindows wo)
    $ keys
    $ laTiles
    $ spLayout
    $ cSpaces c ! getSpaceRef sr
  ; showFloat = wFloats wo ! getSpaceRef sr
  ; f c = if c then (floats ++) else id
  }
  return (f showFloat tileds, f (not showFloat) [])

spaceWindows :: (RefSpace a) => a -> X11State [Window]
spaceWindows sr = do
  wo <- getWorld
  return
    $ concatMap toList
    $ elems
    $ filterWithKey (\tr _ -> sameSpace sr tr)
    $ wTiles wo

refreshMapping :: (RefSpace a) => a -> X11State ()
refreshMapping sr = do
  wo <- getWorld
  d  <- getDisplay
  if sameSpace sr $ wFocus wo
     then do
       (v, h) <- partitionSpace sr
       liftIO $ do
         -- can't use restack when windows have children
         -- mapM_ (lowerWindow d) v
         mapM_ (unmapWindow d) h
         mapM_ (mapWindow d) v
       updateX11Focus
     else spaceWindows sr >>= liftIO . mapM_ (unmapWindow d)

refreshFocusMapping :: X11State ()
refreshFocusMapping = getFocusTileM >>= refreshMapping

modifyTileWindows :: (DQ Window -> DQ Window) -> TileRef -> X11State ()
modifyTileWindows f = modifyWorld . $(upd 'wTiles) . adjust f

modifyAllTileWindows :: (TileRef -> DQ Window -> DQ Window) -> X11State ()
modifyAllTileWindows = modifyWorld . $(upd 'wTiles) . mapWithKey

modifyFocusWindows :: (DQ Window -> DQ Window) -> X11State ()
modifyFocusWindows f = getWorld >>= modifyTileWindows f . getFocusTile

emptyWorld :: Config -> World
emptyWorld c = World
  { wFocuses = fmap spStartTile $ cSpaces c
  , wFocus   = spStartTile $ cSpace c $ cStartSpace c
  , wTiles   = union floats $ emptyWLayout empty spLayout spacesList
  , wHistory = emptyHist
  , wMode    = MNormal
  , wKeyMode = cStartMode c
  , wFloats  = fmap (const True) $ cSpaces c
  , wTrigger = const $ return ()
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

