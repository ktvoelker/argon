
module Debug where

import State
import Types
import X11

import Control.Monad.Maybe
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans
import Graphics.X11

debugEnabled :: Bool
debugEnabled = True

class Debug m where
  debug :: String -> m ()

dprint :: (Debug m, Show a) => a -> m ()
dprint = debug . show

instance Debug (ReaderT X11Env IO) where
  debug xs =
    if debugEnabled
       then getDisplay >>= liftIO . flip sync False
            >> liftIO (putStrLn xs)
       else return ()

instance Debug (MaybeT (StateT World (ReaderT X11Env IO))) where
  debug = lift . lift . debug

