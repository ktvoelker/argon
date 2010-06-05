
module Init where

import Declare
import Event
import X11

-- TODO
getXInfo :: X11 XInfo
getXInfo = return (XInfo 0 0 0 0)

-- TODO
-- Add standard event handlers to all existing windows.
-- Also add special event handlers so that we find out about new windows and
-- add the standard handlers to them.
initEvents :: X11 ()
initEvents = return ()

