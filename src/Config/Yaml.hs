
module Config.Yaml (config) where

import Declare
import Maths.Unsafe

import Control.Monad
import Control.Monad.Error
import Data.Object
import Data.Object.Yaml
import Language.GroteTrap
import System.Environment

configFile :: ConfigM FilePath
configFile = do
  args <- liftIO $ getArgs
  case args of
    (file : _) -> return file
    _          -> do
      home <- getEnv "HOME"
      return (home ++ "/.kdwmrc")

config :: ConfigM Config
config = configFile >>= decodeFile >>= fromYaml

type Obj = Object String String

{-
 - If this module ever gets finished, quit using Maybe in // and /// and just
 - use ConfigM.
 -}

infixl 2 //, ///

(//) :: Obj -> String -> Maybe Obj
(//) (Mapping xs) x = lookup x xs
(//) _              = Nothing

(///) :: Maybe Obj -> String -> Maybe Obj
(///) m xs = m >>= (// xs)

need :: Maybe Obj -> ConfigM Obj
need Nothing  = throwError "Missing data"
need (Just x) = return x

needSeq :: Maybe Obj -> ConfigM [Obj]
needSeq obj = need >>= \o -> case o of
  Sequence xs -> return xs
  _           -> throwError "Needed a sequence"

toScalar :: Obj -> ConfigM String
toScalar (Scalar x) = return x
toScalar _          = throwError "Needed a scalar"

returnJust :: Maybe a -> ConfigM a
returnJust = maybe (throwError "Unknown error") return

fromYaml :: Maybe Obj -> ConfigM Config
fromYaml obj = do
  ts <- needSeq (obj /// "tables") >>= mapM parseTable
  ss <- needSeq (obj /// "spaces") >>= mapM (parseSpace ts)
  ks <- needSeq (obj /// "keys") >>= mapM parseKey
  start <- need $ obj /// "start"
  return emptyConfig
    { cStartSpace = mkSpaceRef start
    , cSpaces = fromList ss
    , cKeys = fromList ks
    }

parseTable :: Obj -> ConfigM (String, Table a)
parseTable obj = do
  (rows, cols) <- returnJust $ do
    rows <- obj // "rows"
    cols <- obj // "cols"
    return (rows, cols)
  rows' <- parse rows
  cols' <- parse cols
  return Table
    { taRows = rows'
    , taCols = cols'
    }
  where
    parse = mapM (toScalar >=> parseExpr)

parseExpr :: String -> ConfigM (Qty u t x)
parseExpr xs = case reads xs of
  -- TODO parse actual expressions here, not just integers
  ((n, _) : _) -> return $ wrap n
  _            -> throwError "Parse error in expression"

parseSpace :: [(String, Table a)] -> Obj -> ConfigM Workspace
parseSpace ts obj = do
  table <- need (obj // "table") >>= returnJust . flip lookup ts
  tiles <- needSeq (obj // "tiles") >>= mapM parseTile
  throwError "Not implemented"

parseTile :: Obj -> ConfigM (Tile a)
parseTile = throwError "Not implemented"

parseKey :: Obj -> ConfigM ((KeyMask, KeySym), Command)
parseKey = throwError "Not implemented"

