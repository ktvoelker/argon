
module Declare.Attract where

data Attract = Attract
  { xName  :: Maybe String
  , xClass :: Maybe String
  , xTrans :: Maybe Bool
  } deriving (Eq, Ord, Show)

