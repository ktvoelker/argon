
module Maths.Unsafe
  ( PixPosn, PixSpan, PixDiff
  , ChrPosn, ChrSpan, ChrDiff
  , CelPosn, CelSpan, CelDiff
  , SecPosn, SecSpan, SecDiff
  , XY, YX, X, Y
  , Pix, Chr, Cel, Sec
  , Posn, Span, Diff
  , Wrapper(..), wrapXY, wrapYX, convert
  , Add(..), Sub(..), Mul(..), Div(..)
  , (/.), (%.), sum'
  ) where

type N = Int

type XY u t = (u t X, u t Y)
type YX u t = (u t Y, u t X)

type PixPosn x = Posn Pix x
type PixSpan x = Span Pix x
type PixDiff x = Diff Pix x

type ChrPosn x = Posn Chr x
type ChrSpan x = Span Chr x
type ChrDiff x = Diff Chr x

type CelPosn x = Posn Cel x
type CelSpan x = Span Cel x
type CelDiff x = Diff Cel x

type SecPosn = Posn Sec X
type SecSpan = Span Sec X
type SecDiff = Diff Sec X

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
newtype Posn t x = Posn N deriving (Enum, Eq, Ord, Show)

-- A size
newtype Span t x = Span N deriving (Enum, Eq, Ord, Show)

-- A difference
newtype Diff t x = Diff N deriving (Enum, Eq, Ord, Show)

class Wrapper w where
  unwrap :: w x -> N
  wrap   :: N -> w x

wrapXY :: (Wrapper w) => N -> N -> (w X, w Y)
wrapXY x y = (wrap x, wrap y)

wrapYX :: (Wrapper w) => N -> N -> (w Y, w X)
wrapYX y x = (wrap y, wrap x)

convert :: (Wrapper w1, Wrapper w2) => w1 x1 -> w2 x2
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
  :: (Wrapper a, Wrapper b, Wrapper c) => a x -> b x -> c x
anyPlus a b = wrap $ (unwrap a) + (unwrap b)
anyMinus a b = wrap $ (unwrap a) - (unwrap b)
anyAbsMinus a b = wrap $ abs $ (unwrap a) - (unwrap b)

class Add a b c where
  infixl 6 +.
  (+.) :: a -> b -> c

class Sub a b c where
  infixl 6 -.
  (-.) :: a -> b -> c

class Mul a b c | a b -> c where
  infixl 7 *.
  (*.) :: a -> b -> c

class Div a b c | a b -> c where
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

instance Sub (Posn t x) (Posn t x) (Span t x) where
  (-.) = anyAbsMinus

instance Sub (Posn t x) (Posn t x) (Diff t x) where
  (-.) = anyMinus

instance Add (Posn t x) (Span t x) (Posn t x) where
  (+.) = anyPlus

instance Sub (Posn t x) (Span t x) (Posn t x) where
  (-.) = anyMinus

instance Add (Span t x) (Span t x) (Span t x) where
  (+.) = anyPlus

instance Sub (Span t x) (Span t x) (Span t x) where
  (-.) = anyAbsMinus

instance Sub (Span t x) (Span t x) (Diff t x) where
  (-.) = anyMinus

instance Mul (Span t x) N (Span t x) where
  (*.) a b = wrap $ (unwrap a) * b

instance Div (Span t x) (Span t x) N where
  (/%.) a b = (unwrap a) `divMod` (unwrap b)

instance Div (Span t x) N (Span t x) where
  (/%.) a b = case (unwrap a) `divMod` b of (d, m) -> (wrap d, wrap m)

sum' :: (Wrapper w, Add (w x) (w x) (w x)) => [w x] -> w x
sum' = foldr (+.) $ wrap 0

