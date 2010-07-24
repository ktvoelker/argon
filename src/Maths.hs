
module Maths
  ( PixelPosn , PixelSpan , PixelDiff
  , CellPosn, CellSpan, CellDiff
  , Add(..), Sub(..), Wrap(..)
  ) where

{-
 - posn + posn -> invalid
 - posn - posn -> diff
 - posn + span -> posn
 - posn - span -> posn
 - span + span -> span
 - span - span -> diff
 - diff + diff -> invalid for now (could be diff)
 - diff - diff -> invalid for now (could be diff)
 -}

newtype PixelPosn = PixelPosn Int
newtype PixelSpan = PixelSpan Int
newtype PixelDiff = PixelDiff Int

newtype CellPosn = CellPosn Int
newtype CellSpan = CellSpan Int
newtype CellDiff = CellDiff Int

class Wrap u w | w -> u where
  unwrap :: w -> u
  wrap   :: u -> w

instance Wrap Int PixelPosn where
  unwrap (PixelPosn n) = n
  wrap = PixelPosn

instance Wrap Int PixelSpan where
  unwrap (PixelSpan n) = n
  wrap = PixelSpan

instance Wrap Int PixelDiff where
  unwrap (PixelDiff n) = n
  wrap = PixelDiff

instance Wrap Int CellPosn where
  unwrap (CellPosn n) = n
  wrap = CellPosn

instance Wrap Int CellSpan where
  unwrap (CellSpan n) = n
  wrap = CellSpan

instance Wrap Int CellDiff where
  unwrap (CellDiff n) = n
  wrap = CellDiff

class IsPosn p where
class IsSpan s where
class IsDiff d where

instance IsPosn PixelPosn where
instance IsSpan PixelSpan where
instance IsDiff PixelDiff where

instance IsPosn CellPosn where
instance IsSpan CellSpan where
instance IsDiff CellDiff where

class SameUnits a b where

instance SameUnits PixelPosn PixelPosn where
instance SameUnits PixelPosn PixelSpan where
instance SameUnits PixelPosn PixelDiff where
instance SameUnits PixelSpan PixelPosn where
instance SameUnits PixelSpan PixelSpan where
instance SameUnits PixelSpan PixelDiff where
instance SameUnits PixelDiff PixelPosn where
instance SameUnits PixelDiff PixelSpan where
instance SameUnits PixelDiff PixelDiff where

instance SameUnits CellPosn CellPosn where
instance SameUnits CellPosn CellSpan where
instance SameUnits CellPosn CellDiff where
instance SameUnits CellSpan CellPosn where
instance SameUnits CellSpan CellSpan where
instance SameUnits CellSpan CellDiff where
instance SameUnits CellDiff CellPosn where
instance SameUnits CellDiff CellSpan where
instance SameUnits CellDiff CellDiff where

class (Wrap Int p, Wrap Int s, Wrap Int d)
  => Maths p s d | p -> s d, s -> p d, d -> s p where

instance Maths PixelPosn PixelSpan PixelDiff where

instance Maths CellPosn CellSpan CellDiff where

anyPlus, anyMinus, anyAbsMinus
  :: (Wrap Int a, Wrap Int b, Wrap Int c) => a -> b -> c
anyPlus a b = wrap $ (unwrap a) + (unwrap b)
anyMinus a b = wrap $ (unwrap a) - (unwrap b)
anyAbsMinus a b = wrap $ abs $ (unwrap a) - (unwrap b)

class Add a b c where
  infixl 6 +.
  (+.) :: a -> b -> c

class Sub a b c where
  infixl 6 -.
  (-.) :: a -> b -> c

instance (Maths p s d) => Sub p p s where
  (-.) = anyAbsMinus

instance (Maths p s d) => Sub p p d where
  (-.) = anyMinus

instance (Maths p s d) => Add p s p where
  (+.) = anyPlus

instance (Maths p s d) => Sub p s p where
  (-.) = anyMinus

instance (Maths p s d) => Add s s s where
  (+.) = anyPlus

instance (Maths p s d) => Sub s s s where
  (-.) = anyAbsMinus

instance (Maths p s d) => Sub s s d where
  (-.) = anyMinus

