
module Exec where

import Declare
import Types

import Control.Monad.Trans
import System.Posix.Process

exec :: (MonadIO m) => Exec -> m ()
exec x = do
  _ <- liftIO $ forkProcess (executeFile (exProg x) True (exArgs x) Nothing)
  return ()

