
module History (History, emptyHist, histBack, histFwd, histGo) where

import Fields
import Types

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

