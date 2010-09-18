
module Config where

import Declare

import qualified Config.Manual as CM
import qualified Config.Yaml   as CY

data ConfigMode =
    ConfigManual
  | ConfigYaml
  deriving (Eq, Ord, Show)

config :: ConfigM
config = chooseConfig ConfigManual

chooseConfig :: ConfigMode -> ConfigM
chooseConfig ConfigManual = CM.config
chooseConfig ConfigYaml   = CY.config

