
module Init where

import Declare
import Event

-- TODO
getXInfo :: IO XInfo
getXInfo = return (XInfo 0 0 0 0)

-- TODO
-- Add standard event handlers to all existing windows.
-- Also add special event handlers so that we find out about new windows and
-- add the standard handlers to them.
initEvents :: IO ()
initEvents = return ()

