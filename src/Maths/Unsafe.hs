
-- inspired by Dimensional library

module Maths.Unsafe where

import Prelude

data O
data N

data U posn span diff free

type UPosn = U O N N N
type USpan = U N O N N
type UDiff = U N N O N
type UFree = U N N N O

newtype Qty u t x = Qty Int deriving (Enum, Eq, Ord, Show)

class Add a b c | a b -> c
class Sub a b c | a b -> c
class Mul a b c | a b -> c
class Div a b c | a b -> c

{-
 - posn + posn -> invalid
 - posn - posn -> diff
 - posn + span -> posn
 - posn - span -> posn
 - span + span -> span
 - span - span -> diff
 - diff + diff -> invalid for now (could be diff)
 - diff - diff -> invalid for now (could be diff)
 - span * free -> span
 - span / span -> free
 - span / free -> span
 - span % span -> free
 - span % free -> span
 - abs diff    -> span
 -}

instance Sub UPosn UPosn UDiff
instance Add UPosn USpan UPosn
instance Sub UPosn USpan UPosn
instance Add USpan USpan USpan
instance Sub USpan USpan UDiff
instance Add UFree UFree UFree
instance Sub UFree UFree UFree
instance Mul USpan UFree USpan
instance Mul UFree UFree UFree
instance Div USpan USpan UFree
instance Div USpan UFree USpan
instance Div UFree UFree UFree

wrap :: Int -> Qty u t x
wrap = Qty

posn :: Int -> Qty UPosn t x
posn = wrap

span :: Int -> Qty USpan t x
span n = if n < 0 then error "Negative span" else wrap n

diff :: Int -> Qty UDiff t x
diff = wrap

free :: Int -> Qty UFree t x
free = wrap

unwrap :: Qty u t x -> Int
unwrap (Qty n) = n

convert :: Qty u1 t1 x1 -> Qty u2 t2 x2
convert = wrap . unwrap

fix1 :: (Int -> Int) -> Qty u1 t1 x1 -> Qty u2 t2 x2
fix1 f = wrap . f . unwrap

fix2 :: (Int -> Int -> Int) -> Qty u1 t1 x1 -> Qty u2 t2 x2 -> Qty u3 t3 x3
fix2 f a b = wrap $ f (unwrap a) (unwrap b)

fix22 :: (Int -> Int -> (Int, Int)) -> Qty u1 t1 x1 -> Qty u2 t2 x2
      -> (Qty u3 t3 x3, Qty u3 t3 x3)
fix22 f a b = case f (unwrap a) (unwrap b) of (a, b) -> (wrap a, wrap b)

infixl 6 +., -.
infix  7 /%.
infixl 7 *., /., %.

(+.) :: (Add a b c) => Qty a t x -> Qty b t x -> Qty c t x
(+.) = fix2 (+)

(-.) :: (Sub a b c) => Qty a t x -> Qty b t x -> Qty c t x
(-.) = fix2 (-)

(*.) :: (Mul a b c) => Qty a t x -> Qty b t x -> Qty c t x
(*.) = fix2 (*)

(/%.) :: (Div a b c) => Qty a t x -> Qty b t x -> (Qty c t x, Qty c t x)
(/%.) = fix22 divMod

(/.) :: (Div a b c) => Qty a t x -> Qty b t x -> Qty c t x
(/.) a b = fst $ a /%. b

(%.) :: (Div a b c) => Qty a t x -> Qty b t x -> Qty c t x
(%.) a b = snd $ a /%. b

sum' :: (Add u u u) => [Qty u t x] -> Qty u t x
sum' = foldr (+.) $ wrap 0

abs' :: Diff t x -> Span t x
abs' = fix1 abs

type Posn t x = Qty UPosn t x
type Span t x = Qty USpan t x
type Diff t x = Qty UDiff t x
type Free t x = Qty UFree t x

data X
data Y

type XY u t = (Qty u t X, Qty u t Y)
type XYPosn t = XY UPosn t
type XYSpan t = XY USpan t
type XYDiff t = XY UDiff t

wrapXY :: Int -> Int -> XY u t
wrapXY x y = (wrap x, wrap y)

posnXY :: Int -> Int -> XYPosn t
posnXY = wrapXY

spanXY :: Int -> Int -> XYSpan t
spanXY x y = wrapXY (abs x) (abs y)

diffXY :: Int -> Int -> XYDiff t
diffXY = wrapXY

instance Num (Free t x) where
  (+) = fix2 (+)
  (*) = fix2 (*)
  (-) = fix2 (-)
  abs = fix1 abs
  signum = fix1 signum
  fromInteger = wrap . fromInteger

instance Real (Free t x) where
  toRational = toRational . unwrap

instance Integral (Free t x) where
  quotRem = fix22 quotRem
  divMod = fix22 divMod
  toInteger = toInteger . unwrap

