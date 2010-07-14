
module Main (main) where

import Config
import Event
import Info
import Init
import State
import X11

xMain :: X11 ()
xMain = do
  xi <- getXInfo
  initEvents
  let config' = runInfo config xi in do
    liftIO $ print config'
    eventLoop config' $ emptyWorld config'

main :: IO ()
main = runX11 xMain ":0.0"

