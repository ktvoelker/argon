
module Command (runCommand) where

import Debug
import Declare
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
import Graphics.X11.Xlib.Extras

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
      w <- getFocusWindow
      liftIO $ killClient d w
      return ()
    CKeyMode mr     -> do
      c <- getConfig
      w <- getWorld
      let kms = cKeys c
      -- Ungrab the old keymap.
      lift $ lift $ ungrabKeyMap $ heirLookup (wKeyMode w) kms
      -- Grab the new keymap.
      putWorld w { wKeyMode = mr }
      lift $ lift $ grabKeyMap $ heirLookup mr kms
    CShowFloat yes  -> do
      tr <- getFocusTileM
      setShowFloat tr yes
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
          refreshFocusSpace
    (CFocus tq) -> do
      c  <- getConfig
      wo <- getWorld
      case evalTileQuery tq c wo of
        [] -> return ()
        ((tr, h) : _) -> when (getFocusTile wo /= tr) $ do
          modifyWorld $ $(upd 'wHistory) $ maybe (histGo tr) const h
          setFocusTile tr
          refreshFocusSpace

