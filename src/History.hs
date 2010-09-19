
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

histBack :: History a -> (Maybe a, History a)
histBack h = case back h of
  []       -> (Nothing, h)
  (b : bs) -> (Just b, $(upd 'fwd) (b :) h { back = bs })

histFwd :: History a -> (Maybe a, History a)
histFwd h = case fwd h of
  []       -> (Nothing, h)
  (b : bs) -> (Just b, $(upd 'back) (b :) h { fwd = bs })

histGo :: a -> History a -> History a
histGo x h = h { fwd = [], back = x : back h }

