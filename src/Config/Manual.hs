
module Config.Manual (config) where

import Declare

config :: ConfigM
config = do
  w <- asks width
  h <- asks height
  let w' = w /. free 2
      h' = h /. free 2
      t = Table { taRows = [h', h'], taCols = [w', w'] }
      a = Tile { tiPos = (posnXY 0 0), tiSpan = (spanXY 1 1) }
      b = Tile { tiPos = (posnXY 1 0), tiSpan = (spanXY 1 1) }
      c = Tile { tiPos = (posnXY 0 1), tiSpan = (spanXY 1 1) }
      d = Tile { tiPos = (posnXY 1 1), tiSpan = (spanXY 1 1) }
      e = Tile { tiPos = (posnXY 0 0), tiSpan = (spanXY 1 1) }
      f = Tile { tiPos = (posnXY 1 0), tiSpan = (spanXY 1 1) }
      g = Tile { tiPos = (posnXY 0 1), tiSpan = (spanXY 2 1) }
  return emptyConfig
    { cStartSpace = main
    , cSpaces = fromList
      [ (main, Workspace
          { spLayout = Layout
            { laTable = t
            , laTiles =
                fromList
                $ zipWith (\n t -> (mkTileRef main n, t))
                  ["a", "b", "c", "d"] [a, b, c, d]
            }
          , spStatus = emptyStatusbar
          , spStartTile = mkTileRef main "a"
          }
        )
      , (two, Workspace
          { spLayout = Layout
            { laTable = t
            , laTiles =
                fromList
                $ zipWith (\n t -> (mkTileRef two n, t))
                  ["e", "f", "g"] [e, f, g]
            }
          , spStatus = emptyStatusbar
          , spStartTile = mkTileRef two "g"
          }
        )
      ]
    , cKeys = fromList
      [ ((0, xK_KP_Right), CFocusDir DRight)
      , ((0, xK_KP_Up), CFocusDir DUp)
      , ((0, xK_KP_Left), CFocusDir DLeft)
      , ((0, xK_KP_Down), CFocusDir DDown)
      , ((0, xK_1), CSpace main)
      , ((0, xK_2), CSpace two)
      , ((0, xK_q), CQuit)
      , ((0, xK_x), CKill)
      ]
    , cAttracts = []
    }
  where
    main = mkSpaceRef "main"
    two  = mkSpaceRef "two"

