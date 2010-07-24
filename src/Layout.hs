
module Layout where

import Declare
import Prelude hiding (span)

realPos, realSpan :: Table -> Tile -> (Nat, Nat)
realPos  ta ti = pos  $ realTile ta ti
realSpan ta ti = span $ realTile ta ti

realTile :: Table -> Tile -> Tile
realTile
  Table { rows = rs, cols = cs }
  Tile { pos = (pc, pr), span = (sc, sr) } =
  Tile { pos = (px, py), span = (sx, sy) }
  where
    (bcs, acs) = splitAt pc cs
    (brs, ars) = splitAt pr rs
    px = sum bcs
    py = sum brs
    sx = sum $ take sc $ acs
    sy = sum $ take sr $ ars

