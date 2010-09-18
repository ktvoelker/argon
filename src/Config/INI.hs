
module Config.INI (config) where

import Declare
import Maths.Unsafe

import Control.Monad
import Control.Monad.Error
import Control.Monad.Reader
import Data.ConfigFile
import Graphics.X11
import System.Environment

defaultFile :: FilePath
defaultFile = ".kdwmrc"

optStart, optTable, optRows, optCols :: OptionSpec
optStart = "start"
optTable = "table"
optRows  = "rows"
optCols  = "cols"

sectTable, sectSpace :: String
sectTable = "table"
sectSpace = "space"

sectGlobal, sectKeys :: SectionSpec
sectGlobal = "global"
sectKeys = "keys"

masks :: Map String KeyMask
masks = fromList
  [ ("S", shiftMask)
  , ("L", lockMask)
  , ("C", controlMask)
  , ("1", mod1Mask)
  , ("2", mod2Mask)
  , ("3", mod3Mask)
  , ("4", mod4Mask)
  , ("5", mod5Mask)
  ]

configFile :: ConfigM FilePath
configFile = liftIO $ do
  args <- getArgs
  case args of
    (file : _) -> return file
    _          -> do
      home <- getEnv "HOME"
      return (home ++ "/" ++ defaultFile)

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
  eInput <- configFile >>= liftIO . readfile emptyCP { optionxform = id }
  mapErrorT (>>= return . either (Left . show) Right) $ do
    input <- eInput
    let sectionNames = map (\n -> (words n, n)) $ sections input
    tableNames <- findSections sectTable sectionNames
    spaceNames <- findSections sectSpace sectionNames
    tables <- mapM (mapSndM $ getTable input) tableNames
    let tableMap = fromList tables
    spaces <-
      mapM
        (\(n, s) -> getSpace input tableMap n s >>= return . (n, ))
        spaceNames
    keys <- options input sectKeys >>= mapM (getKey input)
    start <- get input sectGlobal optStart
    return emptyConfig
      { cSpaces = fromList $ map (mapFst mkSpaceRef) spaces
      , cKeys = fromList keys
      , cStartSpace = mkSpaceRef start
      }

ni :: ConfigM' a
ni = throwError (OtherProblem "Not implemented", "")

readM :: (Read a) => String -> String -> ConfigM' a
readM want xs = case reads xs of
  ((x, _) : _) -> return x
  _            -> parseError ("Expected " ++ want)

returnJust :: String -> Maybe a -> ConfigM' a
returnJust err Nothing  = parseError err
returnJust _   (Just x) = return x

type ConfigM' a = ErrorT CPError (ReaderT XInfo IO) a

parseIntegerList :: String -> ConfigM' [Int]
parseIntegerList = mapM (readM "integer") . words

getTable :: ConfigParser -> SectionSpec -> ConfigM' (Table t)
getTable cp sect = do
  rows <- f optRows
  cols <- f optCols
  return Table
    { taRows = map (convert . free) rows
    , taCols = map (convert . free) cols
    }
  where
    f opt = get cp sect opt >>= parseIntegerList

getSpace
  :: ConfigParser
  -> Map String (Table Pix)
  -> String
  -> SectionSpec
  -> ConfigM' Workspace
getSpace cp tables name sect = do
  table <- get cp sect optTable >>= returnJust "table" . flip lookup tables
  start <- get cp sect optStart >>= return . ref
  tileNames <-
    options cp sect >>= return . filter (not . (`elem` [optStart, optTable]))
  tiles <- mapM (\n -> getTile cp sect n >>= return . (ref n, )) tileNames
  return Workspace
    { spLayout = Layout
      { laTable = table
      , laTiles = fromList tiles
      }
    , spStatus = emptyStatusbar
    , spStartTile = start
    }
  where
    ref = mkTileRef $ mkSpaceRef name

getTile :: ConfigParser -> SectionSpec -> OptionSpec -> ConfigM' (Tile a)
getTile cp sect opt = do
  nums <- get cp sect opt >>= parseIntegerList
  case nums of
    [px, py, sx, sy] -> return Tile
      { tiPos  = posnXY px py
      , tiSpan = spanXY sx sy
      }
    _                -> parseError "Expected four integers"

getKey :: ConfigParser -> OptionSpec -> ConfigM' ((KeyMask, KeySym), Command)
getKey cp opt = do
  str <- get cp sectKeys opt
  case reverse $ words opt of
    []         -> parseError "Invalid key"
    sym : mods -> do
      let sym' = stringToKeysym sym
      mods' <- mapM (returnJust "mask" . flip lookup masks) mods
      cmd <- getCommand str
      return ((foldr (.|.) 0 mods', sym'), cmd)

getCommand :: String -> ConfigM' Command
getCommand = getCommand' . words

getCommand' :: [String] -> ConfigM' Command
getCommand' xs = case xs of
  (cmd : args) -> returnJust "command" (lookup cmd commands) >>= ($ args)
  _            -> parseError "Expected command"

commands :: Map String ([String] -> ConfigM' Command)
commands = fromList
  [ ("focus_dir", cmdFocusDir)
  , ("space",     cmdSpace)
  , ("exec",      cmdExec)
  , ("seq",       cmdSeq)
  , ("quit",      constCmd CQuit)
  , ("kill",      constCmd CKill)
  , ("next_win",  constCmd CNextWin)
  , ("prev_win",  constCmd CPrevWin)
  ]

cmdFocusDir, cmdSpace, cmdExec, cmdSeq :: [String] -> ConfigM' Command

cmdFocusDir [dir] =
  returnJust "direction" (lookup dir dirs) >>= return . CFocusDir
cmdFocusDir _ = parseError "`focus_dir' expects one direction"

dirs :: Map String Dir
dirs = fromList
  [ ("up",    DUp)
  , ("down",  DDown)
  , ("left",  DLeft)
  , ("right", DRight)
  ]

cmdSpace [space] = return $ CSpace $ mkSpaceRef space
cmdSpace _       = parseError "`space' expects one argument"

cmdExec (prog : args) = return $ CExec Exec { exProg = prog, exArgs = args }
cmdExec _             = parseError "`exec' expects at least one argument"

cmdSeq = f >=> return . CSeq
  where
    f args = do
      x'  <- getCommand' x
      xs' <- f xs
      return (x' : xs')
      where
        (x, xs) = break (== ";") args

constCmd :: Command -> a -> ConfigM' Command
constCmd cmd = const $ return cmd

