
module Command (runCommand) where

import Debug
import Declare
import Declare.Access
import Dir
import Event.Listen
import Exec
import Fields
import History
import State
import Tile
import Tile.Query
import Types
import X11

import Control.Monad
import Data.Maybe
import Data.Set (delete)
import Graphics.X11.Xlib.Extras

import qualified Data.Set as Set

runCommand, runCommand' :: Command -> X11State ()

runCommand cmd = do
  wo <- getWorld
  case wMode wo of
    MNormal -> runCommand' cmd
    _       -> return ()

runCommand' cmd = do
  dprint cmd
  case cmd of
    CQuit           -> quitState
    (CSeq xs)       -> mapM_ runCommand xs
    (CExec x)       -> exec x
    CNextWin        -> getFocusTileM >>= nextWin
    CPrevWin        -> getFocusTileM >>= prevWin
    CKill           -> do
      d <- getDisplay
      w <- getNonRootFocusWindow
      whenJust w $ ignore . liftIO . killClient d
    CEnableKeys  xs -> runKeyModeCommand $ flip (Set.fold insert) xs
    CDisableKeys xs -> runKeyModeCommand $ flip (Set.fold delete) xs
    CHideFloat   -> setShowFloat False
    CShowFloat   -> setShowFloat True
    CMove from to breadth depth -> do
      c  <- getConfig
      wo <- getWorld
      let
      { sing  = maybeToList . listToMaybe
      ; from' =
          map fst
          $ (if breadth == AllTiles then id else sing)
          $ evalTileQuery from c wo
      ; to' =
          map fst
          $ sing
          $ evalTileQuery to c wo
      ; popWins =
          if depth == TopWindow
             then fmap maybeToList . removeWin
             else \tr ->
               fmap (map fromJust . takeWhile isJust)
               $ sequence
               $ repeat (removeWin tr)
      }
      debug "Move windows (from, to, and windows):"
      dprint from'
      dprint to'
      -- Reverse the window list before adding them to the destination.
      -- This is good in a few likely cases:
      -- 1. If moving all the windows from one tile, the windows will end up
      --    in their initial order.
      -- 2. If moving windows from many tiles, the top window will be from
      --    the first query result, rather than the last.
      case to' of
        [] -> return ()
        (tr : _) -> do
          fmap concat (mapM popWins from') >>= mapM_ (addWin tr) . reverse
          debug "Moved all windows"
          refreshFocusSpace
          debug "Refresh done after window move"
    (CFocus tq) -> do
      c  <- getConfig
      wo <- getWorld
      let old = getFocusTile wo
      case evalTileQuery tq c wo of
        [] -> return ()
        ((tr, h) : _) -> when (old /= tr) $ do
          modifyWorld $ $(upd 'wHistory) $ maybe (histGo tr) const h
          setFocusTile tr
          refreshSpace old
          refreshFocusSpace

runKeyModeCommand :: (Set ModeRef -> Set ModeRef) -> X11State ()
runKeyModeCommand f = do
  c <- getConfig
  w <- getWorld
  let mode0 = wKeyMode w
  let mode1 = f mode0
  let keys0 = cAllKeys c $ mode0
  let keys1 = cAllKeys c $ mode1
  -- Ungrab the old keymap.
  lift $ lift $ ungrabKeyMap keys0
  -- Grab the new keymap.
  putWorld w { wKeyMode = mode1 }
  lift $ lift $ grabKeyMap keys1

