
module Declare (
    module Declare,
    module Declare.Attract,
    module Declare.Common,
    module Declare.Layout,
    module Declare.Statusbar,
    module Declare.Workspace,
    module Graphics.X11
  ) where

import Declare.Attract
import Declare.Common
import Declare.Layout
import Declare.Statusbar
import Declare.Workspace

import qualified Data.Map as Map
import Graphics.X11 hiding (EventType)

class HasLayout a t where
  layout :: a -> Layout t

instance HasLayout Statusbar Chr where
  layout = stLayout

instance HasLayout Workspace Pix where
  layout = spLayout

data EventType =
  EReady | ECreate | EDestroy | ESpace | EFocus deriving (Enum, Eq, Ord, Show)

data Config = Config
  { spaces     :: Map Name Workspace
  , startSpace :: Name
  , floatMod   :: Modifier
  , keys       :: Map (Modifier, KeySym) Command
  , events     :: Map EventType Command
  } deriving (Eq, Ord, Show)

data Dir = DUp | DDown | DLeft | DRight deriving (Enum, Eq, Ord, Show)

data Command =
    CFocusDir   Dir
  | CFocusName  Name Bool
  | CFocusQuery Attract
  | CSpace      Name
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
  { spaces     = Map.empty
  , startSpace = error "No start space"
  , floatMod   = mod1Mask
  , keys       = Map.empty
  , events     = Map.empty
  }

