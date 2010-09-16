
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
    (CSeq xs)       -> mapM_ runCommand xs
    (CFocusDir dir) -> focusDir dir
    (CExec x)       -> exec x
    _               -> return ()

