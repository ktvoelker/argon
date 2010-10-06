
module Declare
  ( module Declare
  , module Declare.Attract
  , module Declare.Common
  , module Declare.Layout
  , module Declare.Statusbar
  , module Declare.Workspace
  ) where

import Declare.Attract
import Declare.Common
import Declare.Layout
import Declare.Statusbar
import Declare.Workspace

import Control.Monad.Error
import Control.Monad.Reader

import qualified Data.Map as Map

class HasLayout a t r | a -> t r, t -> a r, r -> a t where
  layout :: a -> Layout t r

instance HasLayout Statusbar Chr StatusRef where
  layout = stLayout

instance HasLayout Workspace Pix TileRef where
  layout = spLayout

data Trigger =
    TReady
  | TSpace SpaceRef
  | TFocus TileRef
  deriving (Eq, Ord, Show)

type KeyMap = Map (KeyMask, KeySym) Command

mkKeyHeir :: Map ModeRef ([ModeRef], KeyMap) -> Heir ModeRef KeyMap KeyMap
mkKeyHeir h = mkHeir h empty $ unionWith $ \a b -> CSeq [a, b]

cStartKeys :: Config -> KeyMap
cStartKeys c = heirLookup (cStartMode c) (cKeys c)

data Config = Config
  { cSpaces     :: Map SpaceRef Workspace
  , cStartSpace :: SpaceRef
  , cFloatMask  :: KeyMask
  , cKeys       :: Heir ModeRef KeyMap KeyMap
  , cStartMode  :: ModeRef
  , cIgnoreMask :: KeyMask
  , cTriggers   :: Map Trigger Command
  , cAttracts   :: [(Attract, TileRef)]
  } deriving (Show)

data Dir = DUp | DDown | DLeft | DRight deriving (Enum, Eq, Ord, Show)

data TileQuery =
  -- A tile.
    QAbsolute TileRef
  -- A tile on the focused space, by name.
  | QRelative (Maybe String)
  -- The most-recently-focused tile of a space.
  | QSpace    SpaceRef
  -- The focused tile.
  | QCurrent
  | QHistBack
  | QHistFwd
  | QDir      Dir
  | QDisjunct [TileQuery]
  | QEmptiest TileQuery
  deriving (Eq, Ord, Show)

data Breadth = FirstTile | AllTiles deriving (Enum, Eq, Ord, Show)

data Depth = TopWindow | AllWindows deriving (Enum, Eq, Ord, Show)

data Command =
    CMove       TileQuery TileQuery Breadth Depth
    CFocus      TileQuery
  | CExec       Exec
  | CSeq        [Command]
  | CKeyMode    ModeRef
  | CShowFloat  Bool
  | CQuit
  | CKill
  | CNextWin
  | CPrevWin
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
  , cKeys       = mkKeyHeir Map.empty
  , cStartMode  = error "No start mode"
  , cTriggers   = Map.empty
  , cIgnoreMask = lockMask .|. mod2Mask .|. mod3Mask .|. mod5Mask
  , cAttracts   = []
  }

