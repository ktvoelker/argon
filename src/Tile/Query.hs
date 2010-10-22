
module Tile.Query (evalTileQuery) where

import Declare
import Dir
import History
import State
import Types

import Data.List hiding (filter, lookup)
import Data.Maybe

type TileQueryResult = [(TileRef, Maybe (History TileRef))]

evalTileQuery :: TileQuery -> Config -> World -> TileQueryResult
evalTileQuery tq c wo = nubBy (\a b -> fst a == fst b) $ eval tq c wo

nonHist :: TileRef -> TileQueryResult
nonHist = nonHists . (: [])

nonHists :: [TileRef] -> TileQueryResult
nonHists = map (, Nothing)

evalSR :: Config -> World -> QuerySpaceRef -> [SpaceRef]
evalSR c _ QRAllSpaces     = keys $ cSpaces c
evalSR _ w QRCurSpace      = [getSpaceRef $ wFocus w]
evalSR c _ (QROneSpace sr) = filter (== sr) $ keys $ cSpaces c

evalTR :: Config -> World -> SpaceRef -> QueryTileRef -> [TileRef]
evalTR c w sr QRAllTiles = mkFloatRef sr : evalTR c w sr QRAllNonFloatTiles
evalTR c _ sr QRAllNonFloatTiles =
  concatMap (keys . laTiles . spLayout)
  $ maybeToList
  $ lookup sr
  $ cSpaces c
evalTR _ w sr QRCurTile =
  maybeToList
  $ lookup sr
  $ wFocuses w
evalTR _ w sr (QROneTile n) = case lookup tr $ wTiles w of
  Nothing -> []
  Just _  -> [tr]
  where
    tr = mkTileFloatRef sr n

eval :: TileQuery -> Config -> World -> TileQueryResult
eval (QRef sr tr) c wo = nonHists $ do
  sr' <- evalSR c wo sr
  evalTR c wo sr' tr
eval QHistBack _ wo = evalHist histBack wo
eval QHistFwd _ wo = evalHist histFwd wo
eval (QDir dir) c wo =
  nonHists $ maybeToList $ followDir c dir $ getFocusTile wo
eval (QDisjunct tqs) c wo = tqs >>= \tq -> eval tq c wo
eval (QDifference as bs) c wo =
  deleteFirstsBy (\a b -> fst a == fst b) as' bs'
  where
    as' = eval as c wo
    bs' = eval bs c wo
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

