
module Attract (attract) where

import Declare
import Declare.Access
import Debug
import State
import Tile.Query
import Types
import X11

import Control.Monad
import Data.Maybe
import Foreign.C.String
import Graphics.X11
import Graphics.X11.Xlib.Extras

-- Determine where a window should be placed.
attract :: Window -> X11State TileRef
attract win = do
  c <- getConfig
  wo <- getWorld
  att <-
    -- Keep only the first result, if any.
    fmap listToMaybe $
    -- Check all attractions against the given window.
    fmap (map snd) $ filterM (attractOne win . fst) $ cAttracts c
  -- Return the attracted tile if one was found.
  -- Otherwise, return the focused tile.
  debug "Attracted tile:"
  dprint att
  return
    $ fromMaybe (getFocusTile wo)
    $ att >>= (\tq -> fmap fst $ listToMaybe $ evalTileQuery tq c wo)

attractOne :: Window -> Attract -> X11State Bool
attractOne win att = fmap (all id) $ sequence $ map (($ att) . ($ win)) ways

stringWay :: Atom -> (a -> String -> Bool) -> Window -> a -> X11State Bool
stringWay atom pred win want = getWindowAtom atom win >>= return . (pred want)

maybeWay :: (Attract -> Maybe a) -> (Window -> a -> X11State Bool)
         -> Window -> Attract -> X11State Bool
maybeWay proj pred win att =
  fromMaybe (return True) $ proj att >>= Just . pred win

eitherWay
  :: (Window -> a -> X11State Bool)
  -> (Window -> b -> X11State Bool)
  -> Window -> Either a b -> X11State Bool
eitherWay f g win = either (f win) (g win)

spaceWay :: Window -> SpaceRef -> X11State Bool
spaceWay = locWay sameSpace

tileWay :: Window -> TileRef -> X11State Bool
tileWay = locWay (==)

locWay :: (a -> TileRef -> Bool) -> Window -> a -> X11State Bool
locWay f win ref = do
  wo <- getWorld
  return $ case findWindow wo win of
    Nothing -> False
    Just tr -> f ref tr

ways :: [Window -> Attract -> X11State Bool]
ways = [ maybeWay xName  $ stringWay wM_NAME          (==)
       , maybeWay xClass $ stringWay wM_CLASS         (==)
       , maybeWay xTrans $ stringWay wM_TRANSIENT_FOR notEmpty
       , maybeWay xFocus $ eitherWay spaceWay tileWay
       ]
  where
    notEmpty want str = want == not (null str)

