
module Main (main) where

import qualified Config
import Debug
import Event
import Info
import Init
import State
import Types
import X11

import Command

import System.Environment

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
main = do
  dStr <- catch (getEnv "DISPLAY") $ const $ return ":0.0"
  runX11 xMain dStr $ error "Not configured"

