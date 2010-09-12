
module Attract (attract) where

import Declare
import State
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
  c <- lift $ getConfig
  wo <- getWorld
  att <- lift $ lift $
    -- Keep only the first result, if any.
    fmap listToMaybe $
    -- Check all attractions against the given window.
    fmap (map snd) $ filterM (attractOne win . fst) $ cAttracts c
  -- Return the attracted tile if one was found.
  -- Otherwise, return the focused tile.
  return $ fromMaybe (getFocusTile wo) att

attractOne :: Window -> Attract -> X11 Bool
attractOne win att = anyM $ map (($ att) . ($ win)) ways

-- TODO Don't assume the string encoding in the TP matches ours.
stringWay :: Atom -> (a -> String -> Bool) -> Window -> a -> X11 Bool
stringWay atom pred win want =
  getDisplay
  >>= \d -> lift $
    getTextProperty d win atom
    >>= peekCString . tp_value
    >>= return . (pred want)

maybeWay :: (Attract -> Maybe a) -> (Window -> a -> X11 Bool)
         -> Window -> Attract -> X11 Bool
maybeWay proj pred win att =
  fromMaybe (return False) $ proj att >>= Just . pred win

ways :: [Window -> Attract -> X11 Bool]
ways = [ maybeWay xName  $ stringWay wM_NAME          (==)
       , maybeWay xClass $ stringWay wM_CLASS         (==)
       , maybeWay xTrans $ stringWay wM_TRANSIENT_FOR notEmpty
       ]
  where
    notEmpty want str = want == not (null str)

anyM :: [X11 Bool] -> X11 Bool
anyM []       = return False
anyM (x : xs) = do
  b <- x
  if b then return True else anyM xs

