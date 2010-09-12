
module Config (config) where

import Declare

config :: Reader XInfo Config
config = do
  w <- asks width
  h <- asks height
  let w' = w /. free 2
      t = Table { taRows = [h], taCols = [w', w'] }
      a = Tile { tiPos = (posnXY 0 0), tiSpan = (spanXY 1 1) }
      b = Tile { tiPos = (posnXY 1 0), tiSpan = (spanXY 1 1) }
  return emptyConfig
    { cStartSpace = main
    , cSpaces = fromList
      [ (main, Workspace
          { spLayout = Layout
            { laTable = t
            , laTiles = fromList [(tileA, a), (tileB, b)]
            }
          , spStatus = emptyStatusbar
          , spStartTile = tileA
          })
      ]
    , cKeys = fromList [((0, xK_space), CFocusDir DRight)]
    , cAttracts = []
    }
  where
    main = mkSpaceRef "main"
    tileA = mkTileRef main $ Just "a"
    tileB = mkTileRef main $ Just "b"

