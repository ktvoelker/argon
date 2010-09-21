
-- inspired by Dimensional library

module Maths.Unsafe where

import Prelude

data UPosn
data USpan
data UDiff
data UFree

class ShowType a where
  showType :: a -> String

showsType :: (ShowType a) => a -> ShowS
showsType dum = (showType dum ++)

instance ShowType UPosn where
  showType _ = "posn"

instance ShowType USpan where
  showType _ = "span"

instance ShowType UDiff where
  showType _ = "diff"

instance ShowType UFree where
  showType _ = "free"

newtype Qty u t x = Qty Int deriving (Enum, Eq, Ord)

instance (ShowType u, ShowType t, ShowType x) => Show (Qty u t x) where
  showsPrec p (Qty n) =
    ('<' :)
    . showsType (dum :: t) . spc
    . showsType (dum :: x) . spc
    . showsType (dum :: u) . spc
    . showsPrec p n
    . ('>' :)
    where
      spc = (' ' :)
      dum = undefined

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

wrap :: (Integral i) => i -> Qty u t x
wrap = Qty . fromIntegral

posn :: (Integral i) => i -> Qty UPosn t x
posn = wrap . fromIntegral

span :: (Integral i) => i -> Qty USpan t x
span n = if n < 0 then error "Negative span" else wrap $ fromIntegral n

diff :: (Integral i) => i -> Qty UDiff t x
diff = wrap

free :: (Integral i) => i -> Qty UFree t x
free = wrap

unwrap :: (Num n) => Qty u t x -> n
unwrap (Qty n) = fromIntegral n

convert :: Qty u1 t1 x1 -> Qty u2 t2 x2
convert = fix1 id

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

instance ShowType X where
  showType _ = "x"

instance ShowType Y where
  showType _ = "y"

type XY u t = (Qty u t X, Qty u t Y)
type XYPosn t = XY UPosn t
type XYSpan t = XY USpan t
type XYDiff t = XY UDiff t

wrapXY :: (Integral i) => i -> i -> XY u t
wrapXY x y = (wrap x, wrap y)

posnXY :: (Integral i) => i -> i -> XYPosn t
posnXY = wrapXY

spanXY :: (Integral i) => i -> i -> XYSpan t
spanXY x y = wrapXY (abs x) (abs y)

diffXY :: (Integral i) => i -> i -> XYDiff t
diffXY = wrapXY

instance (ShowType t, ShowType x) => Num (Free t x) where
  (+) = fix2 (+)
  (*) = fix2 (*)
  (-) = fix2 (-)
  abs = fix1 abs
  signum = fix1 signum
  fromInteger = wrap . fromInteger

instance (ShowType t, ShowType x) => Real (Free t x) where
  toRational = toRational . unwrap

instance (ShowType t, ShowType x) => Integral (Free t x) where
  quotRem = fix22 quotRem
  divMod = fix22 divMod
  toInteger = toInteger . unwrap

