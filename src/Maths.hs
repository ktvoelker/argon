
module Maths (module Maths, module Maths.Unsafe) where

import Maths.Unsafe hiding
  ( fix1, fix2, fix22, unwrap, convert
  , O, N, U, UPosn, USpan, UDiff, Qty(..)
  , Add, Sub, Mul, Div
  )

data Pix
data Chr
data Cel
data Sec

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

wrapXY :: Int -> Int -> XY u t
wrapXY x y = (wrap x, wrap y)

