
module Main (main) where

import Config
import Declare
import Info
import State

main = print $ runInfo config (XInfo 0 0 0 0)

