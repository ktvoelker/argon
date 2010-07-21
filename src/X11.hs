
module X11 (
    lift, liftIO, liftM, liftM2,
    X11, X11Env, display, dispScr, withDispScr, runX11
  ) where

import Control.Monad.Reader
import Graphics.X11

type X11 a = ReaderT X11Env IO a

data X11Env = X11Env
  { xDisplay :: Display
  } deriving (Eq, Ord, Show)

display :: (MonadReader X11Env m) => m Display
display = ask >>= return . xDisplay

dispScr :: (MonadReader X11Env m) => m (Display, ScreenNumber)
dispScr = display >>= \d -> return (d, defaultScreen d)

withDispScr :: (MonadReader X11Env m)
            => (Display -> ScreenNumber -> m a) -> m a
withDispScr f = dispScr >>= uncurry f

runX11 :: X11 a -> String -> IO a
runX11 x11 dStr = do
  disp <- openDisplay dStr
  ret  <- runReaderT x11 X11Env { xDisplay = disp }
  closeDisplay disp
  return ret

