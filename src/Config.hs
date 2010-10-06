
module Config where

import Declare

import qualified Config.INI as CI

data ConfigMode =
  ConfigINI
  deriving (Eq, Ord, Show)

config :: ConfigM Config
config = chooseConfig ConfigINI

chooseConfig :: ConfigMode -> ConfigM Config
chooseConfig ConfigINI    = CI.config

