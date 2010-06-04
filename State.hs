
module State where

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

data State = State
  { spaces :: Map Name SSpace
  , timers :: PQueue Timer
  } deriving (Show)

data SSpace = SSpace
  { focus  :: Either Window Name
  , tiles  :: Map Name (Queue Window)
  , floats :: Stack Window
  , status :: Map Name String
  } deriving (Show)

emptyState :: State
emptyState = State
  { spaces = empty
  , timers = empty
  }

