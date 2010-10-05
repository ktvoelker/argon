
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

data EventType =
  EReady | ECreate | EDestroy | ESpace | EFocus deriving (Enum, Eq, Ord, Show)

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
  , cEvents     :: Map EventType Command
  , cAttracts   :: [(Attract, TileRef)]
  } deriving (Show)

data Dir = DUp | DDown | DLeft | DRight deriving (Enum, Eq, Ord, Show)

data Command =
    CFocusDir   Dir
  | CFocusName  TileRef Bool
  | CFocusQuery Attract
  | CSpace      SpaceRef
  | CExec       Exec
  | CSeq        [Command]
  | CKeyMode    ModeRef
  -- Remember the focused tile, execute a command, and then move the top
  -- window from the remembered tile to the current tile.
  | CPut        Command
  | CQuit
  | CKill
  | CNextWin
  | CPrevWin
  | CHistBack
  | CHistFwd
  | CFocusFloat
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
  , cEvents     = Map.empty
  , cIgnoreMask = lockMask .|. mod2Mask .|. mod3Mask .|. mod5Mask
  , cAttracts   = []
  }

