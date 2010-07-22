
module Main (main) where

import qualified Config
import Debug
import Event
import Info
import Init
import State
import X11

xMain :: X11 ()
xMain = do
  debug "Get X info"
  xi <- getXInfo
  debug "Init events"
  initEvents
  debug "Run config"
  let config' = runInfo Config.config xi in do
    dprint config'
    debug "Event loop"
    localConfig (const config') eventLoop
    debug "Done"

main :: IO ()
main = runX11 xMain ":0.0" $ error "Not configured"

