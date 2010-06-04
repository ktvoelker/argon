
module Info (Info, runInfo, get) where

data Info i a = Info (i -> a)

runInfo :: Info i a -> i -> a
runInfo (Info f) = f

get :: (i -> a) -> Info i a
get f = Info (\i -> f i)

instance Monad (Info i) where
  (Info f) >>= g = Info (\i -> runInfo (g $ f i) i)
  return a       = Info (\_ -> a)

