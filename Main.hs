
module Main (main) where

import Action
import Config
import Declare
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
    eventLoop config' emptyState

main :: IO ()
main = runX11 xMain ":0.0"

