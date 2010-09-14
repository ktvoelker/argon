
module Debug (debug, dprint, dwprint) where

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

syncEnabled :: Bool
syncEnabled = False

debug :: (MonadIO m, MonadReader X11Env m) => String -> m ()
debug xs = do
  if syncEnabled
     then getDisplay >>= liftIO . flip sync False
     else return ()
  if debugEnabled
     then liftIO $ putStrLn xs
     else return ()

dprint :: (MonadIO m, MonadReader X11Env m, Show a) => a -> m ()
dprint = debug . show

class Wrap u w | u -> w where
  wrap   :: u -> w
  unwrap :: w -> u

wrapped :: (Wrap u w) => (w -> a) -> u -> a
wrapped = (. wrap)

dwprint :: (Wrap u w, Show w, MonadIO m, MonadReader X11Env m) => u -> m ()
dwprint = wrapped dprint

newtype ShowKeySym = ShowKeySym KeySym

instance Wrap KeySym ShowKeySym where
  wrap = ShowKeySym
  unwrap (ShowKeySym k) = k

instance Show ShowKeySym where
  show = keysymToString . unwrap

