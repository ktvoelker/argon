
module Config.INI (config) where

import Declare

import Control.Monad.Error
import Control.Monad.Reader
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

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (a, c) = (f a, c)

mapSnd :: (a -> b) -> (c, a) -> (c, b)
mapSnd f (c, a) = (c, f a)

mapSndM :: (Monad m) => (a -> m b) -> (c, a) -> m (c, b)
mapSndM f (c, a) = do
  b <- f a
  return (c, b)

parseError :: String -> ConfigM' a
parseError err = throwError (ParseError err, "")

findSections :: String -> [([String], String)] -> ConfigM' [(String, String)]
findSections kind sections = do
  let found = map (mapFst tail) $ filter ((== kind) . head . fst) sections
  if any ((/= 1) . size . fst) found
     then parseError ("Invalid " ++ kind ++ " name")
     else return $ map (mapFst head) found

config :: ConfigM Config
config = do
  eInput <- configFile >>= liftIO . readfile emptyCP
  mapErrorT (>>= return . either (Left . show) Right) $ do
    input <- eInput
    let sectionNames = map (\n -> (words n, n)) $ sections input
    tableNames <- findSections "table" sectionNames
    spaceNames <- findSections "space" sectionNames
    tables <- mapM (mapSndM $ getTable input) tableNames
    spaces <- mapM (mapSndM $ getSpace input tables) spaceNames
    ni

ni :: ConfigM' a
ni = throwError (OtherProblem "Not implemented", "")

type ConfigM' a = ErrorT CPError (ReaderT XInfo IO) a

getTable :: ConfigParser -> String -> ConfigM' (Table t)
getTable cp sect = do
  opts <- options cp sect
  ni

getSpace :: ConfigParser -> [(String, Table t)] -> String -> ConfigM' Workspace
getSpace _ _ _ = ni

