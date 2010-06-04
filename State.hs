
module State where

import Data.Queue.Class
import Data.Queue.PQueue
import Data.Queue.Queue
import Data.Queue.Stack
import Graphics.X11

import Types

data Timer = Timer Nat (State -> State) (Maybe String)

instance Ord Timer where
  compare (Timer a _ _) (Timer b _ _) = compare a b

instance Eq Timer where
  a == b = compare a b == EQ

instance Show Timer where
  showsPrec n (Timer t _ x) =
    ("Timer " ++) . showsPrec n t . (" " ++) . showsPrec n x

instance (Show e) => Show (Queue e) where
  showsPrec n q = ("fromList " ++) . showsPrec n (toList q)

instance (Show e) => Show (Stack e) where
  showsPrec n q = ("fromList " ++) . showsPrec n (toList q)

data State = State
  { spaces :: Assoc Name SSpace
  , timers :: PQueue Timer
  } deriving (Show)

data SSpace = SSpace
  { focus  :: Either Window Name
  , tiles  :: Assoc Name (Queue Window)
  , floats :: Stack Window
  , status :: Assoc Name String
  } deriving (Show)

