
module Layout where

import Declare
import Maths.Unsafe
import Prelude hiding (span)

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

