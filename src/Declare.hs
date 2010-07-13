
module Declare (
    module Declare,
    module Declare.Common,
    module Declare.Layout,
    module Declare.Statusbar,
    module Declare.Workspace,
    module Graphics.X11
  ) where

import Declare.Common
import Declare.Layout    hiding (name)
import Declare.Statusbar hiding (name, layout)
import Declare.Workspace hiding (name, layout)
import qualified Declare.Layout    as DL
import qualified Declare.Statusbar as DS
import qualified Declare.Workspace as DW

import qualified Data.Map as Map
import Graphics.X11 hiding (EventType)

class HasName a where
  name   :: a -> Name

class HasLayout a where
  layout :: a -> Layout

instance HasName Tile where
  name = DL.name

instance HasName Statusbar where
  name = DS.name

instance HasLayout Statusbar where
  layout = DS.layout

instance HasName Workspace where
  name = DW.name

instance HasLayout Workspace where
  layout = DW.layout

data EventType =
  EReady | ECreate | EDestroy | ESpace | EFocus deriving (Enum, Eq, Ord, Show)

data Config = Config
  { spaces   :: [Workspace]
  , floatMod :: Modifier
  , keys     :: Map (Modifier, KeySym) Command
  , events   :: Map EventType Command
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
  { spaces   = []
  , floatMod = mod1Mask
  , keys     = Map.empty
  , events   = Map.empty
  }

