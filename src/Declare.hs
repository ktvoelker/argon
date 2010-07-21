
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
import Declare.Statusbar hiding (layout)
import Declare.Workspace hiding (layout)
import qualified Declare.Layout    as DL
import qualified Declare.Statusbar as DS
import qualified Declare.Workspace as DW

import qualified Data.Map as Map
import Graphics.X11 hiding (EventType)

class HasLayout a where
  layout :: a -> Layout

instance HasLayout Statusbar where
  layout = DS.layout

instance HasLayout Workspace where
  layout = DW.layout

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
  { width      :: Nat
  , height     :: Nat
  , fontWidth  :: Nat
  , fontHeight :: Nat
  } deriving (Eq, Ord, Show)

emptyConfig :: Config
emptyConfig = Config
  { spaces     = Map.empty
  , startSpace = error "No start space"
  , floatMod   = mod1Mask
  , keys       = Map.empty
  , events     = Map.empty
  }

