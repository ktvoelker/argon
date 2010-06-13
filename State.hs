
module State where

import Action
import Declare
import Types
import X11

import Control.Monad.Maybe
import Control.Monad.Reader
import Control.Monad.State
import Graphics.X11

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

-- Find timers in the World that have expired and add their actions to the list
-- of actions to perform.
-- TODO
extractTimers :: X11State [Action] -> X11State [Action]
extractTimers = id

-- Execute the actions, including any expired timers in the World.
actX11State :: X11State [Action] -> X11State ()
actX11State = (>>= lift . lift . mapM_ act) . extractTimers

data Timer = Timer Nat (X11State [Action]) (Maybe String)

instance Ord Timer where
  compare (Timer a _ _) (Timer b _ _) = compare a b

instance Eq Timer where
  a == b = compare a b == EQ

instance Show Timer where
  showsPrec n (Timer t _ x) =
    ("Timer " ++) . showsPrec n t . (" " ++) . showsPrec n x

data World = World
  { wSpaces :: Map Name WSpace
  , wTimers :: PQueue Timer
  , wFocus  :: Name
  } deriving (Show)

data WSpace = WSpace
  { wsFocus  :: Either Window Name
  , wsTiles  :: Map Name (Queue Window)
  , wsFloats :: Stack Window
  , wsStatus :: Map Name String
  } deriving (Show)

-- TODO
emptyWorld :: Config -> World
emptyWorld _ = World
  { wSpaces = empty
  , wTimers = empty
  , wFocus  = ""
  }

