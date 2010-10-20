
module Tile.Query (evalTileQuery) where

import Declare
import Dir
import History
import State
import Types

import Data.List hiding (lookup)
import Data.Maybe

type TileQueryResult = [(TileRef, Maybe (History TileRef))]

evalTileQuery :: TileQuery -> Config -> World -> TileQueryResult
evalTileQuery tq c wo = nubBy (\a b -> fst a == fst b) $ eval tq c wo

nonHist :: TileRef -> TileQueryResult
nonHist = nonHists . (: [])

nonHists :: [TileRef] -> TileQueryResult
nonHists = map (, Nothing)

eval :: TileQuery -> Config -> World -> TileQueryResult
eval (QAbsolute tr) _ _ = nonHist tr
eval (QRelative xs) _ wo =
  case lookup tr $ wTiles wo of
    Nothing -> []
    Just _  -> nonHist tr
  where
    tr = mkTileFloatRef (getSpaceRef $ getFocusTile wo) xs
eval (QSpace sr) _ wo = nonHist $ getLocalFocus wo sr
eval QCurrent _ wo = nonHist $ getFocusTile wo
eval QHistBack _ wo = evalHist histBack wo
eval QHistFwd _ wo = evalHist histFwd wo
eval (QDir dir) c wo =
  nonHists $ maybeToList $ followDir c dir $ getFocusTile wo
eval (QDisjunct tqs) c wo = tqs >>= \tq -> eval tq c wo
eval (QEmptiest tq) c wo =
  -- TODO is the built-in sortBy stable?
  map snd
  $ sortBy (\a b -> compare (fst a) (fst b))
  $ map (\(tr, h) -> (size $ wTiles wo ! tr, (tr, h)))
  $ eval tq c wo

evalHist
  :: (TileRef -> History TileRef -> Maybe (TileRef, History TileRef))
  -> World
  -> TileQueryResult
evalHist f wo = case f (wFocus wo) (wHistory wo) of
  Nothing  -> []
  Just (tr, h) -> [(tr, Just h)]

