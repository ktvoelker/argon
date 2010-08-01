
module Declare (
    module Declare,
    module Declare.Attract,
    module Declare.Common,
    module Declare.Layout,
    module Declare.Statusbar,
    module Declare.Workspace
  ) where

import Declare.Attract
import Declare.Common
import Declare.Layout
import Declare.Statusbar
import Declare.Workspace

import qualified Data.Map as Map

class HasLayout a t where
  layout :: a -> Layout t

instance HasLayout Statusbar Chr where
  layout = stLayout

instance HasLayout Workspace Pix where
  layout = spLayout

data EventType =
  EReady | ECreate | EDestroy | ESpace | EFocus deriving (Enum, Eq, Ord, Show)

data Config = Config
  { cSpaces     :: Map Name Workspace
  , cStartSpace :: Name
  , cFloatMask  :: KeyMask
  , cKeys       :: Map (KeyMask, KeySym) Command
  , cIgnoreMask :: KeyMask
  , cEvents     :: Map EventType Command
  } deriving (Eq, Ord, Show)

data Dir = DUp | DDown | DLeft | DRight deriving (Enum, Eq, Ord, Show)

data Command =
    CFocusDir   Dir
  | CFocusName  Name Bool
  | CFocusQuery Attract
  | CSpace      Name
  | CExec       Exec
  | CSeq        [Command]
  deriving (Eq, Ord, Show)

data XInfo = XInfo
  { width      :: PixSpan X
  , height     :: PixSpan Y
  , fontWidth  :: PixSpan X
  , fontHeight :: PixSpan Y
  } deriving (Eq, Ord, Show)

emptyConfig :: Config
emptyConfig = Config
  { cSpaces     = Map.empty
  , cStartSpace = error "No start space"
  , cFloatMask  = mod1Mask
  , cKeys       = Map.empty
  , cEvents     = Map.empty
  , cIgnoreMask = lockMask .|. mod2Mask .|. mod3Mask .|. mod5Mask
  }

