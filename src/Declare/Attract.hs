
module Declare.Attract where

import Types

data Attract = Attract
  { xName  :: Maybe String
  , xClass :: Maybe String
  , xTrans :: Maybe Bool
  , xFocus :: Maybe (Either SpaceRef TileRef)
  } deriving (Eq, Ord, Show)

