
module Main (main) where

import qualified Config
import Event
import Info
import Init
import State
import X11

xMain :: X11 ()
xMain = do
  xi <- getXInfo
  initEvents
  let config' = runInfo Config.config xi in do
    liftIO $ print config'
    localConfig (const config') eventLoop

main :: IO ()
main = runX11 xMain ":0.0" $ error "Not configured"

