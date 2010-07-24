
module Command where

import Action
import Declare
import Fields
import Layout
import Maths.Unsafe
import State
import Types
import X11

import Data.List
import Data.Maybe

data LookDir = LookDir
  -- axis rearranges an (x,y) pair so that the coordinate on the axis
  -- parallel to the gaze is first.
  --
  -- In order for this to work with the Maths module, it is implemented as
  -- a rotation of the entire coordinate space, so that Y is known as X, and
  -- vice-versa.
  { axis :: forall u t. (Wrapper (u t)) => XY u t -> XY u t
  -- dir is the one-dimensional direction of the gaze.
  , dir  :: Ordering
  }

-- toSide, given the position and size (along the axis of movement) of a
-- neighboring tile, returns the coordinate of the side of that tile which
-- the gaze will encounter first.
--
-- fromSide, given the position and size (along the axis of movement) of the
-- origin of the gaze, returns the coordinate of the side of that tile which
-- the gaze will encounter first.
--
-- Note that a coordinate value always denotes a row or column of the
-- layout table, whereas a tile side is conceptually between two rows or
-- two columns of the layout table. The coordinate attributed to a tile side
-- is always the coordinate of the first row or column after that side in
-- the positive direction along that axis.
toSide, fromSide :: Ordering -> (Posn t x, Span t x) -> Posn t x
toSide   LT = uncurry (+.)
toSide   GT = uncurry const
fromSide LT = uncurry const
fromSide GT = uncurry (+.)

-- Get the position and size of a tile along either the parallel or
-- perpendicular axes, relative to the gaze.
parPS :: LookDir -> Tile t -> (Posn t X, Span t X)
parPS ld tile = (fst $ axis ld $ pos tile, fst $ axis ld $ size tile)

perPS :: LookDir -> Tile t -> (Posn t Y, Span t Y)
perPS ld tile = (snd $ axis ld $ pos tile, snd $ axis ld $ size tile)

-- True iff the second tile borders the first and is visible when gazing in
-- the given direction.
borders :: LookDir -> Tile t -> Tile t -> Bool
borders ld from to = 
  fromSide s fromPS == toSide s toPS
  && compare (fst fromPS) (fst toPS) == s
  where
    fromPS = parPS ld from
    toPS   = parPS ld to
    s      = dir ld

xAxis, yAxis :: (Wrapper (u t)) => XY u t -> XY u t
xAxis        = id
yAxis (x, y) = (convert y, convert x)

lookDir :: Map Dir LookDir
lookDir = fromList
  [ (DUp, LookDir
    { axis = yAxis
    , dir  = LT
    })
  , (DDown, LookDir
    { axis = yAxis
    , dir  = GT
    })
  , (DLeft, LookDir
    { axis = xAxis
    , dir  = LT
    })
  , (DRight, LookDir
    { axis = xAxis
    , dir  = GT
    })
  ]

-- Assuming the given tiles share an edge perpendicular to the given gaze,
-- what is the length of their shared edge?
sharedEdge :: LookDir -> Table p -> Tile t -> Tile t -> Span p Y
sharedEdge ld t a b = shareEnd -. shareBegin
  where
    ap, bp :: Posn p Y
    as, bs :: Span p Y
    (ap, as)   = perPS $ realTile t a
    (bp, bs)   = perPS $ realTile t b
    shareBegin, shareEnd :: Posn p Y
    shareBegin = max ap bp
    shareEnd   = min (ap +. as) (bp +. bs)

runCommand :: Command -> X11State ()

runCommand (CFocusDir dir) = do
  c  <- lift $ lift $ config
  wo <- getWorld
  case wholeFocus wo of
    -- A floating window is focused.
    (_, Left _)      -> return ()  -- TODO
    -- A tile is focused.
    (fsn, Right ftn) -> do
      -- Determine the tile to focus.
      let
        ld          = lookDir ! dir
        space       = spaces c ! fsn
        lay         = spLayout space
        from        = tiles lay ! ftn
        (ftn', ft') =
          -- Pick the bordering tile with the most shared edge.
          snd $ maximumBy (\a b -> compare (fst a) (fst b))
          -- Pair each bordering tile with the length of its shared edge.
          $ map (\to -> (sharedEdge ld (table lay) from $ snd to, to))
          -- Pick out the bordering tiles.
          $ Prelude.filter (borders ld from . snd)
          -- Get all the tiles in the workspace.
          $ toList $ tiles lay
      -- Record the newly-focused tile.
      modifyFocusSpace $ $(upd 'wsFocus) $ const $ Right $ ftn'
      -- Tell X to focus the window atop that tile.
      disp <- display
      act $ AFocus $ topWinOrRoot disp $ wsTiles (wFocusSpace wo) ! ftn'

-- TODO
runCommand _ = return ()

topWinOrRoot :: Display -> Queue Window -> Window
topWinOrRoot disp coll = fromMaybe (defaultRootWindow disp) $ top coll

