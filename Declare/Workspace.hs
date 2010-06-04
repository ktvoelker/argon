
module Declare.Workspace where

import Declare.Common
import Declare.Layout    (Layout)
import Declare.Statusbar (Statusbar)

data Attract = Attract
  { wm_name  :: Maybe String
  , wm_class :: Maybe String
  , wm_trans :: Maybe Bool
  } deriving (Eq, Ord, Show)

data Workspace = Workspace
  { name     :: Name
  , layout   :: Layout
  , status   :: Statusbar
  , attracts :: Assoc Name [Attract]
  } deriving (Eq, Ord, Show)

