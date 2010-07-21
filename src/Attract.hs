
module Attract where

import State
import Types

import Graphics.X11

-- Determine the workspace and tile where a window should be placed.
attract :: Window -> X11State (Name, Maybe Name)
attract w = fail "foo" -- TODO

