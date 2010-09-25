
module X11
  ( asks, lift, liftIO, liftM, liftM2
  , X11, X11Env, getDisplay, getConfig
  , dispScr, withDispScr, localConfig, runX11
  , getRoot
  ) where

import Declare
import Fields

import Control.Monad.Reader
import Graphics.X11

type X11 a = ReaderT X11Env IO a

data X11Env = X11Env
  { xeDisplay :: Display
  , xeConfig :: Config
  } deriving (Eq, Ord, Show)

getDisplay :: (MonadReader X11Env m) => m Display
getDisplay = asks xeDisplay

getConfig :: (MonadReader X11Env m) => m Config
getConfig = asks xeConfig

dispScr :: (MonadReader X11Env m) => m (Display, ScreenNumber)
dispScr = getDisplay >>= \d -> return (d, defaultScreen d)

withDispScr :: (MonadReader X11Env m)
            => (Display -> ScreenNumber -> m a) -> m a
withDispScr f = dispScr >>= uncurry f

getRoot :: (MonadReader X11Env m, MonadIO m) => m Window
getRoot = dispScr >>= liftIO . uncurry rootWindow

localConfig :: (MonadReader X11Env m) => (Config -> Config) -> m a -> m a
localConfig f = local ($(upd 'xeConfig) f)

runX11 :: X11 a -> String -> Config -> IO a
runX11 x11 dStr c = do
  disp <- openDisplay dStr
  ret  <- runReaderT x11 X11Env { xeDisplay = disp, xeConfig = c }
  closeDisplay disp
  return ret

