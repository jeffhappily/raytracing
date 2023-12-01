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
import Data.Text.Lazy
import Data.Text.Lazy.Builder qualified as Builder
import Data.Text.Lazy.Builder.Int (decimal)

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
  negate (V3 a b c) = V3 (-a) (-b) (-c)

newtype Color = Color V3
  deriving newtype (Semiring, Ring)

-- toPpm :: Color -> String
-- toPpm (Color (V3 x y z)) =
--   let r = 256 * x
--       g = 256 * y
--       b = 256 * z
--    in unwords $ fmap (show . floor) [r, g, b] -- TODO: More efficient implementation?
toPpm :: Color -> Text
toPpm (Color (V3 x y z)) =
  let r = floor (256 * x) :: Int
      g = floor (256 * y) :: Int
      b = floor (256 * z) :: Int
      builder = decimal r <> Builder.singleton ' ' <> decimal g <> Builder.singleton ' ' <> decimal b
   in Builder.toLazyText builder
newtype Point3 = Point3 V3
  deriving newtype (Semiring, Ring)
