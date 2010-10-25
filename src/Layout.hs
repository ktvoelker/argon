
module Layout where

import Debug
import Declare
import Declare.Access
import Maths.Unsafe
import X11

import Prelude hiding (span)
import Control.Monad
import Graphics.X11

realPos :: Table t -> Tile Cel -> XYPosn t
realPos  ta ti = tiPos  $ realTile ta ti

realSpan :: Table t -> Tile Cel -> XYSpan t
realSpan ta ti = tiSpan $ realTile ta ti

realTile :: Table t -> Tile Cel -> Tile t
realTile
  Table { taRows = rs, taCols = cs }
  Tile { tiPos = (pc, pr), tiSpan = (sc, sr) } =
  Tile { tiPos = (px, py), tiSpan = (sx, sy) }
  where
    (bcs, acs) = splitAt (unwrap pc) cs
    (brs, ars) = splitAt (unwrap pr) rs
    px = wrap $ unwrap $ sum' bcs
    py = wrap $ unwrap $ sum' brs
    sx = sum' $ take (unwrap sc) $ acs
    sy = sum' $ take (unwrap sr) $ ars

layoutWindow :: TileRef -> Window -> X11 ()
layoutWindow tr win = when (not $ tileIsFloat tr) $ do
  c <- getConfig
  let
    lay = spLayout $ cSpace c tr
    ti  = (laTiles lay) ! tr
    ta  = laTable lay
    (px, py) = realPos ta ti
    (dw, dh) = realSpan ta ti
  debug "Tiling layout (pos, span):"
  dprint $ realPos ta ti
  dprint $ realSpan ta ti
  d <- getDisplay
  liftIO $ moveResizeWindow d win (fi px) (fi py) (fi dw) (fi dh)
  where
    fi :: (Num a) => Qty u t x -> a
    fi = fromIntegral . unwrap

