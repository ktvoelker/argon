
module Fields (upd) where

import Language.Haskell.TH

upd :: Name -> Q Exp
upd n = do
  p0 <- newName "f"
  p1 <- newName "x"
  return $
    LamE [VarP p0, VarP p1] $
    RecUpdE (VarE p1) [(n, AppE (VarE p0) (AppE (VarE n) (VarE p1)))]

data Foo = Foo { foo :: Int }

upd' :: IO ()
upd' = runQ (upd 'foo) >>= print

