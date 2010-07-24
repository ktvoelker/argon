
module Maths.Unsafe
  ( PixPosn, PixSpan, PixDiff
  , ChrPosn, ChrSpan, ChrDiff
  , CelPosn, CelSpan, CelDiff
  , SecPosn, SecSpan, SecDiff
  , XY, YX, X, Y
  , Pix, Chr, Cel, Sec
  , Posn, Span, Diff
  , Wrapper(..), convert
  , Add(..), Sub(..), Mul(..), Div(..)
  , (/.), (%.), sum'
  ) where

type XY u t = (u t Int X, u t Int Y)
type YX u t = (u t Int Y, u t Int X)

type PixPosn x = Posn Pix Int x
type PixSpan x = Span Pix Int x
type PixDiff x = Diff Pix Int x

type ChrPosn x = Posn Chr Int x
type ChrSpan x = Span Chr Int x
type ChrDiff x = Diff Chr Int x

type CelPosn x = Posn Cel Int x
type CelSpan x = Span Cel Int x
type CelDiff x = Diff Cel Int x

type SecPosn = Posn Sec Int X
type SecSpan = Span Sec Int X
type SecDiff = Diff Sec Int X

-- Pixels
data Pix

-- Characters
data Chr

-- Cells (in a layout table)
data Cel

-- Seconds
data Sec

-- X axis
data X

-- Y axis
data Y

-- A position
newtype Posn t a x = Posn a deriving (Enum, Eq, Ord, Show)

-- A size
newtype Span t a x = Span a deriving (Enum, Eq, Ord, Show)

-- A difference
newtype Diff t a x = Diff a deriving (Enum, Eq, Ord, Show)

class Wrapper w where
  unwrap :: w a x -> a
  wrap   :: a -> w a x

convert :: (Wrapper w, Wrapper y) => w a x -> y a x
convert = wrap . unwrap

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
  :: (Num n, Wrapper a, Wrapper b, Wrapper c) => a n x -> b n x -> c n x
anyPlus a b = wrap $ (unwrap a) + (unwrap b)
anyMinus a b = wrap $ (unwrap a) - (unwrap b)
anyAbsMinus a b = wrap $ abs $ (unwrap a) - (unwrap b)

class Add a b c where
  infixl 6 +.
  (+.) :: a -> b -> c

class Sub a b c where
  infixl 6 -.
  (-.) :: a -> b -> c

class Mul a b c where
  infixl 7 *.
  (*.) :: a -> b -> c

class Div a b c where
  infix 7 /%.
  (/%.) :: a -> b -> (c, c)

infixl 7 /., %.
(/.), (%.) :: (Div a b c) => a -> b -> c
(/.) a b = fst $ a /%. b
(%.) a b = snd $ a /%. b

{-
 - posn + posn -> invalid
 - posn - posn -> span or diff
 - posn + span -> posn
 - posn - span -> posn
 - span + span -> span
 - span - span -> span or diff
 - diff + diff -> invalid for now (could be diff)
 - diff - diff -> invalid for now (could be diff)
 - span * free -> span
 - span / span -> free
 - span / free -> span
 - span % span -> free
 - span % free -> span
 -}

instance (Num a) => Sub (Posn t a x) (Posn t a x) (Span t a x) where
  (-.) = anyAbsMinus

instance (Num a) => Sub (Posn t a x) (Posn t a x) (Diff t a x) where
  (-.) = anyAbsMinus

instance (Num a) => Add (Posn t a x) (Span t a x) (Posn t a x) where
  (+.) = anyPlus

instance (Num a) => Sub (Posn t a x) (Span t a x) (Posn t a x) where
  (-.) = anyMinus

instance (Num a) => Add (Span t a x) (Span t a x) (Span t a x) where
  (+.) = anyPlus

instance (Num a) => Sub (Span t a x) (Span t a x) (Span t a x) where
  (-.) = anyAbsMinus

instance (Num a) => Sub (Span t a x) (Span t a x) (Diff t a x) where
  (-.) = anyMinus

instance (Num a) => Mul (Span t a x) a (Span t a x) where
  (*.) a b = wrap $ (unwrap a) * b

instance (Integral a) => Div (Span t a x) (Span t a x) a where
  (/%.) a b = (unwrap a) `divMod` (unwrap b)

instance (Integral a) => Div (Span t a x) a (Span t a x) where
  (/%.) a b = case (unwrap a) `divMod` b of (d, m) -> (wrap d, wrap m)

sum' :: (Num a, Wrapper w, Add (w a x) (w a x) (w a x)) => [w a x] -> w a x
sum' = foldr (+.) $ wrap 0

