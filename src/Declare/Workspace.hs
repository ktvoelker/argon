
module Declare.Workspace where

import Declare.Common
import Declare.Layout    (Layout)
import Declare.Statusbar (Statusbar)

data Attract = Attract
  { xName  :: Maybe String
  , xClass :: Maybe String
  , xTrans :: Maybe Bool
  } deriving (Eq, Ord, Show)

data Workspace = Workspace
  { name     :: Name
  , layout   :: Layout
  , status   :: Statusbar
  , attracts :: Map (Maybe Name) [Attract]
  } deriving (Eq, Ord, Show)

