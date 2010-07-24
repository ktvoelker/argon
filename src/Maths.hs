
module Maths where

data Pixel
data Cell

newtype Posn t a = Posn a
newtype Span t a = Span a
newtype Diff t a = Diff a

class Wrapper a where
  unwrap :: a b -> b
  wrap   :: b -> a b

instance Wrapper (Posn Pixel) where
  unwrap (Posn n) = n
  wrap = Posn

instance Wrapper (Span Pixel) where
  unwrap (Span n) = n
  wrap = Span

instance Wrapper (Diff Pixel) where
  unwrap (Diff n) = n
  wrap = Diff

instance Wrapper (Posn Cell) where
  unwrap (Posn n) = n
  wrap = Posn

instance Wrapper (Span Cell) where
  unwrap (Span n) = n
  wrap = Span

instance Wrapper (Diff Cell) where
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

instance (Num a) => Sub (Posn t a) (Posn t a) (Span t a) where
  (-.) = anyAbsMinus

instance (Num a) => Sub (Posn t a) (Posn t a) (Diff t a) where
  (-.) = anyAbsMinus

