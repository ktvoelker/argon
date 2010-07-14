
module State where

import Action
import Declare
import X11

import Control.Monad.Maybe
import Control.Monad.Reader
import Control.Monad.State

-- The MaybeT determines whether or not the user has asked the program to
-- terminate.
-- The StateT tracks the current runtime state, in the case that the user has
-- not asked the program to terminate.
type X11State a = MaybeT (StateT World (ReaderT X11Env IO)) a

getWorld :: X11State World
getWorld = lift get

putWorld :: World -> X11State ()
putWorld = lift . put

modifyWorld :: (World -> World) -> X11State ()
modifyWorld = lift . modify

quitState :: X11State a
quitState = fail "quit"

runX11State :: World -> X11State a -> X11 (Maybe a, World)
runX11State world = flip runStateT world . runMaybeT

-- Execute the actions.
actX11State :: X11State [Action] -> X11State ()
actX11State = (>>= lift . lift . mapM_ act)

data World = World
  { wSpaces :: Map Name WSpace
  , wFocus  :: Name
  } deriving (Show)

data WSpace = WSpace
  { wsFocus  :: Either Window Name
  , wsTiles  :: Map Name (Queue Window)
  , wsFloats :: Stack Window
  , wsStatus :: Map Name String
  } deriving (Show)

emptyWorld :: Config -> World
emptyWorld c = World
  { wSpaces = fromList $ map emptyWSpace $ spaces c
  , wFocus  = name $ head $ spaces c
  }

emptyWSpace :: Workspace -> (Name, WSpace)
emptyWSpace w = (name w, WSpace
  { wsFocus  = Right $ name $ head $ tiles $ layout w
  , wsTiles  = emptyLayout empty w
  , wsFloats = empty
  , wsStatus = emptyLayout "" $ status w
  })

emptyLayout :: (HasLayout a) => b -> a -> Map Name b
emptyLayout e x = fromList $ map (, e) $ map name $ tiles $ layout x

