
module X11 (
    X11, liftIO, display, runX11
  ) where

import Control.Monad.Reader
import Graphics.X11

type X11 a = ReaderT X11Env IO a

data X11Env = X11Env
  { xDisplay :: Display
  } deriving (Eq, Ord, Show)

display :: (MonadReader X11Env m) => m Display
display = ask >>= return . xDisplay

runX11 :: X11 a -> String -> IO a
runX11 x11 dStr = do
  disp <- openDisplay dStr
  ret  <- runReaderT x11 X11Env { xDisplay = disp }
  closeDisplay disp
  return ret

