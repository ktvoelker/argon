
module Command where

import Debug
import Declare
import Exec
import Focus
import State
import Types

runCommand :: Command -> X11State ()

runCommand cmd = do
  dprint cmd
  case cmd of
    (CFocusDir dir) -> focusDir dir
    (CExec x)       -> exec x
    _               -> return ()

