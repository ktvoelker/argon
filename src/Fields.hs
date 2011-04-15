
module Fields (upd, enter) where

import Control.Monad.State
import Language.Haskell.TH
import Prelude

upd :: Name -> Q Exp
upd n = do
  p0 <- newName "f"
  p1 <- newName "x"
  return $
    LamE [VarP p0, VarP p1] $
    RecUpdE (VarE p1) [(n, AppE (VarE p0) (AppE (VarE n) (VarE p1)))]

enter :: Name -> Q Exp
enter n = do
  s <- newName "s"
  v <- newName "v"
  r <- newName "r"
  w <- newName "w"
  t <- newName "t"
  let u = return (n, VarE w)
  let body = [| runState $(varE s) $(varE v) |]
  lamE [varP s] $ doE 
    [ bindS (varP v) [| gets $(varE n) |]
    , letS [valD (tupP [varP r, varP w]) (normalB body) []]
    , noBindS [| modify $(lamE [varP t] $ recUpdE (varE t) [u]) |]
    , noBindS [| return $(varE r) |]
    ]

data Foo = Foo { foo :: Int }

upd' :: IO ()
upd' = runQ (upd 'foo) >>= print

enter' :: IO ()
enter' = runQ (enter 'foo) >>= print

