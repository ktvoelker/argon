
module Maths (Pix, Cel, Posn, Span, Diff, Wrapper(..), Add(..), Sub(..)) where

data Pix
data Cel

newtype Posn t a = Posn a
newtype Span t a = Span a
newtype Diff t a = Diff a

class Wrapper a where
  unwrap :: a b -> b
  wrap   :: b -> a b

instance Wrapper (Posn t) where
  unwrap (Posn n) = n
  wrap = Posn

instance Wrapper (Span t) where
  unwrap (Span n) = n
  wrap = Span

instance Wrapper (Diff t) where
  unwrap (Diff n) = n
  wrap = Diff

anyPlus, anyMinus, anyAbsMinus
  :: (Num n, Wrapper a, Wrapper b, Wrapper c) => a n -> b n -> c n
anyPlus a b = wrap $ (unwrap a) + (unwrap b)
anyMinus a b = wrap $ (unwrap a) - (unwrap b)
anyAbsMinus a b = wrap $ abs $ (unwrap a) - (unwrap b)

class Add a b c where
  infixl 6 +.
  (+.) :: a -> b -> c

class Sub a b c where
  infixl 6 -.
  (-.) :: a -> b -> c

{-
 - posn + posn -> invalid
 - posn - posn -> span or diff
 - posn + span -> posn
 - posn - span -> posn
 - span + span -> span
 - span - span -> span or diff
 - diff + diff -> invalid for now (could be diff)
 - diff - diff -> invalid for now (could be diff)
 -}

instance (Num a) => Sub (Posn t a) (Posn t a) (Span t a) where
  (-.) = anyAbsMinus

instance (Num a) => Sub (Posn t a) (Posn t a) (Diff t a) where
  (-.) = anyAbsMinus

instance (Num a) => Add (Posn t a) (Span t a) (Posn t a) where
  (+.) = anyPlus

instance (Num a) => Sub (Posn t a) (Span t a) (Posn t a) where
  (-.) = anyMinus

instance (Num a) => Add (Span t a) (Span t a) (Span t a) where
  (+.) = anyPlus

instance (Num a) => Sub (Span t a) (Span t a) (Span t a) where
  (-.) = anyAbsMinus

instance (Num a) => Sub (Span t a) (Span t a) (Diff t a) where
  (-.) = anyMinus

