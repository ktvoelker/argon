
module Declare
  ( module Declare
  , module Declare.Attract
  , module Declare.Common
  , module Declare.Layout
  , module Declare.Workspace
  ) where

import Declare.Attract
import Declare.Common
import Declare.Layout
import Declare.Workspace

import Control.Monad.Error
import Control.Monad.Reader

import qualified Data.Map as Map
import qualified Data.Set as Set

data Trigger =
    TReady
  | TSpace SpaceRef
  | TFocus TileRef
  | TShowFloat SpaceRef Bool
  deriving (Eq, Ord, Show)

type KeyMap = Map (KeyMask, KeySym) Command

data Config = Config
  { cSpaces     :: Map SpaceRef Workspace
  , cStartSpace :: SpaceRef
  , cFloatMask  :: KeyMask
  , cKeys       :: Map ModeRef KeyMap
  , cStartMode  :: Set ModeRef
  , cIgnoreMask :: KeyMask
  , cTriggers   :: Map Trigger Command
  , cAttracts   :: [(Attract, TileQuery)]
  } deriving (Show)

data Dir = DUp | DDown | DLeft | DRight deriving (Enum, Eq, Ord, Show)

data QuerySpaceRef =
  QRAllSpaces | QRCurSpace | QROneSpace SpaceRef
  deriving (Eq, Ord, Show)

data QueryTileRef =
  QRAllTiles | QRAllNonFloatTiles | QRCurTile | QROneTile (Maybe String)
  deriving (Eq, Ord, Show)

data TileQuery =
  QRef QuerySpaceRef QueryTileRef
  | QHistBack
  | QHistFwd
  | QDir      Dir
  | QDisjunct [TileQuery]
  | QDifference TileQuery TileQuery
  | QEmptiest TileQuery
  deriving (Eq, Ord, Show)

data Breadth = FirstTile | AllTiles deriving (Enum, Eq, Ord, Show)

data Depth = TopWindow | AllWindows deriving (Enum, Eq, Ord, Show)

data Command =
    CMove        TileQuery TileQuery Breadth Depth
  | CFocus       TileQuery
  | CExec        Exec
  | CSeq         [Command]
  | CEnableKeys  (Set ModeRef)
  | CDisableKeys (Set ModeRef)
  | CHideFloat
  | CShowFloat
  | CQuit
  | CKill
  | CDelete
  | CNextWin
  | CPrevWin
  | CSpaceMenu
  deriving (Eq, Ord, Show)

data XInfo = XInfo
  { width      :: PixSpan X
  , height     :: PixSpan Y
  , fontWidth  :: PixSpan X
  , fontHeight :: PixSpan Y
  } deriving (Eq, Ord, Show)

type ConfigM a = ErrorT String (ReaderT XInfo IO) a

emptyConfig :: Config
emptyConfig = Config
  { cSpaces     = Map.empty
  , cStartSpace = error "No start space"
  , cFloatMask  = mod1Mask
  , cKeys       = Map.empty
  , cStartMode  = Set.empty
  , cTriggers   = Map.empty
  , cIgnoreMask = lockMask .|. mod2Mask .|. mod3Mask .|. mod5Mask
  , cAttracts   = []
  }

