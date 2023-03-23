module Vec3 (
  V3 (V3),
  Color (Color),
  Point3 (Point3),
  toPpm,
) where

import Data.Semiring (
  Ring (negate),
  Semiring (
    one,
    plus,
    times,
    zero
  ),
 )

data V3
  = V3
      {-# UNPACK #-} !Double
      {-# UNPACK #-} !Double
      {-# UNPACK #-} !Double
  deriving (Show, Eq, Ord)

instance Semiring V3 where
  plus :: V3 -> V3 -> V3
  V3 a b c `plus` V3 a' b' c' =
    V3 (a + a') (b + b') (c + c')

  times :: V3 -> V3 -> V3
  V3 a b c `times` V3 a' b' c' =
    V3 (a * a') (b * b') (c * c')

  zero :: V3
  zero = V3 0 0 0

  one :: V3
  one = V3 1 1 1

instance Ring V3 where
  negate :: V3 -> V3
  negate (V3 a b c) = V3 (- a) (- b) (- c)

newtype Color = Color V3
  deriving newtype (Semiring, Ring)

toPpm :: Color -> String
toPpm (Color (V3 x y z)) =
  let r = 256 * x
      g = 256 * y
      b = 256 * z
   in unwords $ fmap (show . floor) [r, g, b] -- TODO: More efficient implementation?

newtype Point3 = Point3 V3
  deriving newtype (Semiring, Ring)
