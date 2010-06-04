
module Main (main) where

import Action
import Config
import Declare
import Event
import Info
import Init
import State

main = do
  xi <- getXInfo
  initEvents
  let config' = runInfo config xi in do
    print config'
    eventLoop config' emptyState

