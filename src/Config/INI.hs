
module Config.INI (config) where

import Declare
import Fields
import Maths.Unsafe

import Control.Monad
import Control.Monad.Error
import Control.Monad.Reader
import Data.ConfigFile
import Data.List hiding (lookup, null, filter)
import Data.Maybe
import Graphics.X11
import System.Environment

defaultFile :: FilePath
defaultFile = ".argon.ini"

optOn, optReady, optEnter, optShowFloat, optHideFloat :: String
optOn        = "on"
optReady     = "ready"
optEnter     = "enter"
optShowFloat = "show-float"
optHideFloat = "hide-float"

optStart, optStartKeys, optTable, optLayout, optRows, optCols
  , optTile, optName, optClass, optRole, optTransient, optFocus
  :: OptionSpec
optStart     = "start"
optStartKeys = "start_keys"
optTable     = "table"
optLayout    = "layout"
optRows      = "rows"
optCols      = "cols"
optTile      = "tile"
optName      = "name"
optClass     = "class"
optRole      = "role"
optTransient = "transient"
optFocus     = "focus"

sectTable, sectLayout, sectSpace, sectKeys, sectAttract :: String
sectTable   = "table"
sectLayout  = "layout"
sectSpace   = "space"
sectKeys    = "keys"
sectAttract = "attract"

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

mapFstM :: (Monad m) => (a -> m b) -> (a, c) -> m (b, c)
mapFstM f (a, c) = do
  b <- f a
  return (b, c)

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

splitNames :: [String] -> [([String], String)]
splitNames = map (\n -> (words n, n))

config :: ConfigM Config
config = do
  eInput <- configFile >>= liftIO . readfile emptyCP { optionxform = id }
  mapErrorT (>>= return . either (Left . show) Right) $ do
    input <- eInput
    let sectionNames = splitNames $ sections input
    tableNames  <- findSections sectTable sectionNames
    layoutNames <- findSections sectLayout sectionNames
    spaceNames  <- findSections sectSpace sectionNames
    modeNames   <- findSections sectKeys sectionNames
    attNames    <- findSections sectAttract sectionNames
    tables  <- mapM (mapSndM $ getTable input) tableNames
    let tableMap  = fromList tables
    layouts <- mapM (mapSndM $ getLayout input tableMap) layoutNames
    let layoutMap = fromList layouts
    spaces <-
      mapM
        (\(n, s) -> getSpace input layoutMap n s >>= return . (n, ))
        spaceNames
    let spaces' = map (\(n, (s, t)) -> ((n, s), t)) spaces
    let (spaces'', spTriggers) = unzip spaces'
    keys <- mapM (mapSndM (getKeys input) . mapFst mkModeRef) modeNames
    start <- get input sectGlobal optStart
    startKeys <- get input sectGlobal optStartKeys
    atts <- mapM (getAttract input . snd) attNames
    (triggers, _) <- getTriggers globalTriggers input sectGlobal
    return emptyConfig
      { cSpaces     = fromList $ map (mapFst mkSpaceRef) spaces''
      , cKeys       = fromList keys
      , cStartSpace = mkSpaceRef start
      , cStartMode  = fromList $ map mkModeRef $ words startKeys
      , cAttracts   = fromList atts
      , cTriggers   = fromList (triggers ++ concat spTriggers)
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
  -> ConfigM' (Workspace, [(Trigger, Command)])
getSpace cp layouts name sect = do
  layout <-
    get cp sect optLayout
    >>= returnJust "layout" . flip lookup layouts
  let layout' = $(upd 'laTiles) (mapKeys ref) layout
  start <-
    get cp sect optStart
    >>= return . ref
  let sr = mkSpaceRef name
  (triggers, _) <- getTriggers spaceTriggers cp sect
  return
    ( Workspace
      { spLayout = layout'
      , spStatus = emptyStatusbar
      , spStartTile = start
      }
    , map (mapFst ($ sr)) triggers
    )
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

getKeys :: ConfigParser -> SectionSpec -> ConfigM' KeyMap
getKeys cp sect =
  options cp sect
  >>= mapM (getKey cp sect)
  >>= return . fromList

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

emptyAttract :: Attract
emptyAttract =
  Attract
  { xName  = Nothing
  , xClass = Nothing
  , xRole  = Nothing
  , xTrans = Nothing
  , xFocus = Nothing
  }

attractProps :: Map String (Attract -> String -> ConfigM' Attract)
attractProps = fromList
  [ (optName,      \att xs -> return att { xName  = Just xs })
  , (optClass,     \att xs -> return att { xClass = Just xs })
  , (optRole,      \att xs -> return att { xRole  = Just xs })
  , (optTransient, \att -> parseBool >=> \b -> return att { xTrans = Just b })
  , (optFocus,     applyFocusProp)
  ]

bools :: Map String Bool
bools = fromList $
  map (, True)
  [ "yes"
  , "true"
  , "1"
  ]
  ++
  map (, False)
  [ "no"
  , "false"
  , "0"
  ]

parseBool :: String -> ConfigM' Bool
parseBool xs
  | Just b <- lookup xs bools = return b
  | otherwise = parseError "Expected a Boolean value"

applyFocusProp :: Attract -> String -> ConfigM' Attract
applyFocusProp att xs =
  return $ att
  { xFocus =
      Just
        $ either
        (Left . mkSpaceRef)
        (\(s, t) -> Right $ mkTileRef (mkSpaceRef s) t)
        $ parseSpaceOrTileRef xs
  }

getAttract :: ConfigParser -> SectionSpec -> ConfigM' (Attract, TileQuery)
getAttract cp sect = do
  att <-
    foldM (\att (opt, f) -> get cp sect opt >>= f att) emptyAttract
    $ toList
    $ filterWithKey (\opt _ -> has_option cp sect opt) attractProps
  tq  <- get cp sect optTile >>= parseQuery . words
  return (att, tq)

getCommand :: String -> ConfigM' Command
getCommand = getCommand' . words

getCommand' :: [String] -> ConfigM' Command
getCommand' xs = case xs of
  (cmd : args) -> returnJust "command" (lookup cmd commands) >>= ($ args)
  _            -> parseError "Expected command"

commands :: Map String ([String] -> ConfigM' Command)
commands = fromList
  [ ("move",         cmdMove)
  , ("focus",        cmdFocus)
  , ("exec",         cmdExec)
  , ("seq",          cmdSeq)
  , ("enable_keys",  cmdKeys CEnableKeys)
  , ("disable_keys", cmdKeys CDisableKeys)
  , ("show_float",   constCmd CShowFloat)
  , ("hide_float",   constCmd CHideFloat)
  , ("quit",         constCmd CQuit)
  , ("kill",         constCmd CKill)
  , ("next_win",     constCmd CNextWin)
  , ("prev_win",     constCmd CPrevWin)
  ]

cmdMove, cmdFocus, cmdExec, cmdSeq
  :: [String] -> ConfigM' Command

cmdMove = f FirstTile TopWindow
  where
    err = parseError "Not enough arguments to `move'"
    f _ _ [] = err
    f _ d ("broad" : xs) = f AllTiles d xs
    f b _ ("deep" : xs) = f b AllWindows xs
    f b d xs = do
      from' <- from
      to'   <- to
      return $ CMove from' to' b d
      where
        (left, right) = break (== "->") xs
        arrow = not $ null right
        from = if arrow then parseQuery left else return $ QRef QRCurSpace QRCurTile
        to = parseQuery $ if arrow then tail right else left

cmdFocus = parseQuery >=> return . CFocus

staticQueries :: Map String TileQuery
staticQueries = fromList $
  [ ("hist-back", QHistBack)
  , ("hist-fwd",  QHistFwd)
  , ("dir-up",    QDir DUp)
  , ("dir-down",  QDir DDown)
  , ("dir-left",  QDir DLeft)
  , ("dir-right", QDir DRight)
  ]

staticSpaces :: Map String QuerySpaceRef
staticSpaces = fromList $
  [ (".", QRCurSpace)
  , ("*", QRAllSpaces)
  ]

staticTiles :: Map String QueryTileRef
staticTiles = fromList $
  [ (".", QRCurTile)
  , ("*", QRAllTiles)
  , ("+", QRAllNonFloatTiles)
  , ("^", QROneTile Nothing)
  ]

parseQuery, parseQuery' :: [String] -> ConfigM' TileQuery

parseQuery xs = case break (== "-") xs of
  (_, [])  -> parseQuery' xs
  (as, bs) -> do
    as' <- parseQuery as
    bs' <- parseQuery $ drop 1 bs
    return $ QDifference as' bs'

parseQuery' [] = parseError "Expected a tile query"
parseQuery' [xs] = parseLeafQuery xs
parseQuery' ("emptiest:" : xs) = parseQuery' xs >>= return . QEmptiest
parseQuery' xs = mapM parseLeafQuery xs >>= return . QDisjunct

parseLeafQuery :: String -> ConfigM' TileQuery
parseLeafQuery xs
  | Just r <- lookup xs staticTiles = return $ QRef QRCurSpace r
  | Just q <- lookup xs staticQueries = return $ q
  | otherwise = return $ QRef spaceRef tileRef
    where
      (space, tile) = case parseSpaceOrTileRef xs of
        Left xs   -> (".", xs)
        Right tup -> tup
      spaceRef = fromMaybe (QROneSpace $ mkSpaceRef space) $ lookup space staticSpaces
      tileRef  = fromMaybe (QROneTile $ Just tile) $ lookup tile staticTiles

parseSpaceOrTileRef :: String -> Either String (String, String)
parseSpaceOrTileRef xs = if slash then Right (left, tail right) else Left left
  where
    (left, right) = break (== '/') xs
    slash = not $ null right

cmdExec (prog : args) = return $ CExec Exec { exProg = prog, exArgs = args }
cmdExec _             = parseError "`exec' expects at least one argument"

cmdSeq = f >=> return . CSeq
  where
    f []   = return []
    f args = do
      x'  <- getCommand' x
      xs' <- f $ drop 1 xs  -- do not use ``tail'': xs may be empty
      return (x' : xs')
      where
        (x, xs) = break (== ";") args

cmdKeys :: (Set ModeRef -> Command) -> [String] -> ConfigM' Command
cmdKeys _ [] = parseError "Expected at least one key mode argument"
cmdKeys f xs = return $ f $ fromList $ map mkModeRef xs

constCmd :: Command -> a -> ConfigM' Command
constCmd cmd = const $ return cmd

type Triggers a = Map (String, Bool) (Maybe String -> a)

globalTriggers :: Triggers Trigger
globalTriggers = fromList $
  [ ((optReady, False), const TReady)
  ]

spaceTriggers :: Triggers (SpaceRef -> Trigger)
spaceTriggers = fromList $
  [ ((optEnter, False), const TSpace)
  , ((optEnter, True),  \mt sr -> TFocus $ mkTileRef sr $ fromJust mt)
  , ((optShowFloat, False), const $ flip TShowFloat True)
  , ((optHideFloat, False), const $ flip TShowFloat False)
  ]

getTriggers
  :: Triggers a
  -> ConfigParser
  -> SectionSpec
  -> ConfigM' ([(a, Command)], [OptionSpec])
getTriggers trigs cp sect = do
  -- Get all the option names and pair each name with itself split into words.
  opts <- options cp sect >>= return . splitNames
  -- Transform the split version of each option name into a Maybe Trigger.
  let opts1 = map (mapFst f) opts
  -- Partition the options into those which are triggers and the rest.
  let (opts2, non) = partition (isJust . fst) opts1
  -- Keep only the original option names of the non-trigger options.
  let non' = map snd non
  -- Get rid of the Just around each of the Triggers.
  let opts3 = map (mapFst fromJust) opts2
  -- Parse the RHS of each trigger option as a command.
  opts4 <- mapM (mapSndM getCommand) opts3
  -- Return the results.
  return (opts4, non')
  where
    f ["on", ev] = g (ev, False) Nothing
    f ["on", ev, arg] = g (ev, True) $ Just arg
    f _ = Nothing
    g key arg = lookup key trigs >>= return . ($ arg)

