
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

-- Determine the workspace and tile where a window should be placed.
attract :: Window -> X11State (Name, Maybe Name)
attract win = do
  c <- lift config
  wo <- getWorld
  atts <- lift $ lift $
    fmap (filter (isJust . snd)) $
    sequence $ map (\(n, io) -> fmap (n, ) io) $ toList $
    fmap (attractInSpace win) $ cSpaces c
  return $ case atts of
      [(sn, Just tn)] -> (sn, tn)
      _ -> case wholeFocus wo of
        (sn, Left _)   -> (sn, Nothing)
        (sn, Right tn) -> (sn, Just tn)

-- Nothing means not this space.
-- Just Nothing means floating on this space.
-- Just (Just n) means the tile named n.
attractInSpace :: Window -> Workspace -> X11 (Maybe (Maybe Name))
attractInSpace win space =
  fmap (listToMaybe . map snd) $
  filterM (attractOne win . fst) $ attracts space

-- TODO Don't assume the string encoding in the TP matches ours.
stringWay :: Atom -> (a -> String -> Bool) -> Window -> a -> X11 Bool
stringWay atom pred win want =
  display
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

attractOne :: Window -> Attract -> X11 Bool
attractOne win att = anyM $ map (($ att) . ($ win)) ways

