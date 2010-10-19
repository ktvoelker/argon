
module Debug (debug, debug', dprint, dprint', dwprint, dwprint') where

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
syncEnabled = True

debug, debug' :: (MonadIO m, MonadReader X11Env m) => String -> m ()

debug = debug' . (++ "\n")

debug' xs = do
  if syncEnabled
     then getDisplay >>= liftIO . flip sync False
     else return ()
  if debugEnabled
     then liftIO $ putStr xs
     else return ()

dprint, dprint' :: (MonadIO m, MonadReader X11Env m, Show a) => a -> m ()

dprint = debug . show

dprint' = debug' . show

class Wrap u w | u -> w where
  wrap   :: u -> w
  unwrap :: w -> u

wrapped :: (Wrap u w) => (w -> a) -> u -> a
wrapped = (. wrap)

dwprint, dwprint' :: (Wrap u w, Show w, MonadIO m, MonadReader X11Env m) => u -> m ()

dwprint = wrapped dprint

dwprint' = wrapped dprint'

newtype ShowKeySym = ShowKeySym KeySym

instance Wrap KeySym ShowKeySym where
  wrap = ShowKeySym
  unwrap (ShowKeySym k) = k

instance Show ShowKeySym where
  show = keysymToString . unwrap

