
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
    { cStartSpace = "main"
    , cSpaces = fromList
      [ ("main", Workspace
          { spLayout = Layout
            { laTable = t
            , laTiles = fromList [("a", a), ("b", b)]
            }
          , spStatus = emptyStatusbar
          , spAttracts = []
          , spStartTile = Just "a"
          })
      ]
    , cKeys = fromList [((anyModifier, xK_space), CFocusDir DRight)]
    }

