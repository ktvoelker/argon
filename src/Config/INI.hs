
module Config.INI (config) where

import Declare

import Control.Monad.Error
import Data.ConfigFile
import System.Environment

configFile :: ConfigM FilePath
configFile = liftIO $ do
  args <- getArgs
  case args of
    (file : _) -> return file
    _          -> do
      home <- getEnv "HOME"
      return (home ++ "/.kdwmrc")

config :: ConfigM Config
config = do
  eInput <- configFile >>= liftIO . readfile emptyCP
  input  <- mapErrorT (>>= return . either (Left . show) Right) eInput
  throwError "foo"

