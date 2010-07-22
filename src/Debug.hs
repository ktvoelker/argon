
module Debug where

import State
import X11

import Control.Monad.Maybe
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans

debugEnabled :: Bool
debugEnabled = True

class Debug m where
  debug :: String -> m ()

dprint :: (Debug m, Show a) => a -> m ()
dprint = debug . show

instance Debug IO where
  debug = if debugEnabled then putStrLn else const $ return ()

instance Debug (ReaderT X11Env IO) where
  debug = liftIO . debug

instance Debug (MaybeT (StateT World (ReaderT X11Env IO))) where
  debug = lift . lift . liftIO . debug

