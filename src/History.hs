
module History
  (History, emptyHist, histBack, histFwd, histGo, findFirstBack
  ) where

import Fields
import Types

import Data.Maybe
import qualified Data.Set as Set

data History a =
  History
  { fwd  :: [a]
  , back :: [a]
  } deriving (Eq, Ord, Show)

emptyHist :: History a
emptyHist = History { fwd = [], back = [] }

histBack :: a -> History a -> Maybe (a, History a)
histBack from h = case back h of
  []       -> Nothing
  (b : bs) -> Just (b, $(upd 'fwd) (from :) h { back = bs })

histFwd :: a -> History a -> Maybe (a, History a)
histFwd from h = case fwd h of
  []       -> Nothing
  (b : bs) -> Just (b, $(upd 'back) (from :) h { fwd = bs })

histGo :: a -> History a -> History a
histGo from h = h { fwd = [], back = from : back h }

findFirstBack :: (Ord a) => Set a -> History a -> Maybe a
findFirstBack set = listToMaybe . filter (flip Set.member set) . back

