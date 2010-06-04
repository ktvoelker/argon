
module Declare (
    module Declare,
    module Declare.Common,
    module Declare.Layout,
    module Declare.Statusbar,
    module Declare.Workspace
  ) where

import Declare.Common
import Declare.Layout    hiding (name)
import Declare.Statusbar hiding (name, layout)
import Declare.Workspace hiding (name, layout)
import qualified Declare.Layout    as DL
import qualified Declare.Statusbar as DS
import qualified Declare.Workspace as DW

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

