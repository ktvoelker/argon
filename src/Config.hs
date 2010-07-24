
module Config (config) where

import Declare
import Info

config :: Info XInfo Config
config = do
  w <- get width
  h <- get height
  let w' = w /. (2 :: Int)
      t = Table { rows = [h], cols = [w', w'] }
      a = Tile { pos = (wrapXY 0 0), span = (wrapXY 1 1) }
      b = Tile { pos = (wrapXY 1 0), span = (wrapXY 1 1) }
  return emptyConfig
    { startSpace = "main"
    , spaces = fromList
      [ ("main", Workspace
          { spLayout = Layout
            { table = t
            , tiles = fromList [("a", a), ("b", b)]
            }
          , status = emptyStatusbar
          , attracts = []
          , startTile = Just "a"
          })
      ]
    }

