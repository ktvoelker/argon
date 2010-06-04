
module Main (main) where

import Config
import Declare
import Info

main = print $ runInfo config (XInfo 0 0 0 0)

