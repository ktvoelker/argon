
module Config where

import Declare

import qualified Config.Manual as CM
import qualified Config.INI    as CI

data ConfigMode =
    ConfigManual
  | ConfigINI
  deriving (Eq, Ord, Show)

config :: ConfigM Config
config = chooseConfig ConfigINI

chooseConfig :: ConfigMode -> ConfigM Config
chooseConfig ConfigManual = CM.config
chooseConfig ConfigINI    = CI.config

