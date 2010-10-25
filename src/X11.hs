
module X11
  ( asks, lift, liftIO, liftM, liftM2
  , X11, X11Env, getDisplay, getConfig
  , dispScr, withDispScr, localConfig, runX11
  , getRoot, getWindowAtom, getAtom
  ) where

import Declare
import Fields

import Control.Monad.Reader
import Foreign
import Foreign.C.String
import GHC.Ptr
import Graphics.X11
import Graphics.X11.Xlib.Extras

type X11 a = ReaderT X11Env IO a

data X11Env = X11Env
  { xeDisplay :: Display
  , xeConfig :: Config
  } deriving (Show)

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

getAtom :: (MonadReader X11Env m, MonadIO m) => String -> m (Maybe Atom)
getAtom xs = do
  d <- getDisplay
  a <- liftIO $ internAtom d xs True
  return $ if a == none then Nothing else Just a

-- TODO Don't assume the string encoding in the TP matches ours.
getWindowAtom
  :: (MonadReader X11Env m, MonadIO m) => Atom -> Window -> m String
getWindowAtom atom win = do
  disp <- getDisplay
  liftIO $ alloca $ \tp -> do
    status <- xGetTextProperty disp win tp atom
    if status == 0
       then return ""
       else peek tp >>= f . tp_value
  where
    f :: CString -> IO String
    f c_str =
      if c_str == nullPtr
         then return ""
         else peekCString c_str

