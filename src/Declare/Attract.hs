
module Declare.Attract where

import Types

data Attract = Attract
  { xName  :: Maybe String
  , xClass :: Maybe String
  , xTrans :: Maybe Bool
  } deriving (Eq, Ord, Show)

