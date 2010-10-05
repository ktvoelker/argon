
module Config.INI (config) where

import Declare
import Fields
import Maths.Unsafe

import Control.Monad
import Control.Monad.Error
import Control.Monad.Reader
import Data.ConfigFile
import Graphics.X11
import System.Environment

defaultFile :: FilePath
defaultFile = ".argonrc"

optStart, optStartKeys, optTable, optLayout, optRows, optCols, optParents
  :: OptionSpec
optStart     = "start"
optStartKeys = "start_keys"
optTable     = "table"
optLayout    = "layout"
optRows      = "rows"
optCols      = "cols"
optParents   = "parents"

sectTable, sectLayout, sectSpace, sectKeys :: String
sectTable  = "table"
sectLayout = "layout"
sectSpace  = "space"
sectKeys   = "keys"

sectGlobal :: SectionSpec
sectGlobal = "global"

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
    tableNames  <- findSections sectTable sectionNames
    layoutNames <- findSections sectLayout sectionNames
    spaceNames  <- findSections sectSpace sectionNames
    modeNames   <- findSections sectKeys  sectionNames
    tables  <- mapM (mapSndM $ getTable input) tableNames
    let tableMap  = fromList tables
    layouts <- mapM (mapSndM $ getLayout input tableMap) layoutNames
    let layoutMap = fromList layouts
    spaces <-
      mapM
        (\(n, s) -> getSpace input layoutMap n s >>= return . (n, ))
        spaceNames
    keys <- mapM (mapSndM (getKeys input) . mapFst mkModeRef) modeNames
    start <- get input sectGlobal optStart
    startKeys <- get input sectGlobal optStartKeys
    return emptyConfig
      { cSpaces     = fromList $ map (mapFst mkSpaceRef) spaces
      , cKeys       = mkKeyHeir $ fromList keys
      , cStartSpace = mkSpaceRef start
      , cStartMode  = mkModeRef startKeys
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

-- Like Data.ConfigFile.get, but accepts a list of SectionSpecs, returning
-- the option value from the first section which has that option.
getPref :: ConfigParser -> [SectionSpec] -> OptionSpec -> ConfigM' String
getPref cp ss opt = case filter (\s -> has_option cp s opt) ss of
  [] -> parseError
    ("Required option `" ++ opt ++ "' not found in any relevant section.")
  (sect : _) -> get cp sect opt

getLayout
  :: ConfigParser
  -> Map String (Table Pix)
  -> SectionSpec
  -> ConfigM' (Layout Pix String)
getLayout cp tables sect = do
  table <-
    get cp sect optTable
    >>= returnJust "table" . flip lookup tables
  tileNames <-
    options cp sect
    >>= return . filter (not . (`elem` [optStart, optTable]))
  tiles <-
    mapM (\n -> getTile cp sect n >>= return . (n, )) tileNames
  return Layout
    { laTable = table
    , laTiles = fromList tiles
    }

getSpace
  :: ConfigParser
  -> Map String (Layout Pix String)
  -> String
  -> SectionSpec
  -> ConfigM' Workspace
getSpace cp layouts name sect = do
  layout <-
    get cp sect optLayout
    >>= returnJust "layout" . flip lookup layouts
  let layout' = $(upd 'laTiles) (mapKeys ref) layout
  start <-
    get cp sect optStart
    >>= return . ref
  return Workspace
    { spLayout = layout'
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

getKeys :: ConfigParser -> SectionSpec -> ConfigM' ([ModeRef], KeyMap)
getKeys cp sect = do
  parents <-
    if has_option cp sect optParents
       then get cp sect optParents >>= return . words
       else return []
  let parentRefs = map mkModeRef parents
  keys <- options cp sect >>= mapM (getKey cp sect) . filter (/= optParents)
  return (parentRefs, fromList keys)

getKey
  :: ConfigParser
  -> SectionSpec
  -> OptionSpec
  -> ConfigM' ((KeyMask, KeySym), Command)
getKey cp sect opt = do
  str <- get cp sect opt
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
  [ ("focus_dir",   cmdFocusDir)
  , ("space",       cmdSpace)
  , ("exec",        cmdExec)
  , ("seq",         cmdSeq)
  , ("key_mode",    cmdKeyMode)
  , ("put",         cmdPut)
  , ("quit",        constCmd CQuit)
  , ("kill",        constCmd CKill)
  , ("next_win",    constCmd CNextWin)
  , ("prev_win",    constCmd CPrevWin)
  , ("hist_back",   constCmd CHistBack)
  , ("hist_fwd",    constCmd CHistFwd)
  , ("focus_float", constCmd CFocusFloat)
  ]

cmdFocusDir, cmdSpace, cmdExec, cmdSeq, cmdKeyMode, cmdPut
  :: [String] -> ConfigM' Command

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

cmdKeyMode [m] = return $ CKeyMode $ mkModeRef m
cmdKeyMode _   = parseError "`key_mode' expects one argument"

cmdPut = getCommand' >=> return . CPut

constCmd :: Command -> a -> ConfigM' Command
constCmd cmd = const $ return cmd

