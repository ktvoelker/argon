
module Declare.Common
  ( module Declare.Common
  , module Types
  , module Control.Monad.Reader
  , module Data.Bits
  , module Graphics.X11.Types
  ) where

import qualified Ref
import Types

-- These are typically used by configurations. We want the user to only need
-- to import Declare in the Config module.
import Control.Monad.Reader (asks, Reader)
import Data.Bits ((.|.))
import Graphics.X11.Types hiding (EventType)

mkSpaceRef :: String -> SpaceRef
mkSpaceRef = Ref.SpaceRef

mkTileRef :: SpaceRef -> Maybe String -> TileRef
mkTileRef = Ref.TileRef

mkStatusRef :: SpaceRef -> String -> StatusRef
mkStatusRef = Ref.StatusRef

data Table t = Table
  { taRows :: [Span t Y]
  , taCols :: [Span t X]
  } deriving (Eq, Ord, Show)

data Exec = Exec
  { exProg :: String
  , exArgs :: [String]
  } deriving (Eq, Ord, Show)

