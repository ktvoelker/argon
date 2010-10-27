
module Main (main) where

import Config
import Debug
import Event
import Init
import State
import Types
import X11

import Control.Monad.Error
import Control.Monad.Reader
import Graphics.X11.Xlib.Extras
import System.Environment

xMain :: X11 ()
xMain = do
  debug "Get X info"
  xi <- getXInfo
  dprint xi
  debug "Run config"
  config' <- liftIO $ runReaderT (runErrorT config) xi
  dprint config'
  debug "Use config"
  case config' of
    Left err      -> liftIO $ putStrLn err
    Right config' -> localConfig (const config') $ do
      debug "Init events"
      initEvents
      debug "Event loop"
      eventLoop
  debug "Done"

main :: IO ()
main = do
  dStr <- catch (getEnv "DISPLAY") $ const $ return ":0.0"
  runX11 xMain dStr $ error "Not configured"

