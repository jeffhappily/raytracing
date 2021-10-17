module Color (Color (Color)) where

import Vec3 (V3 (..))

newtype Color = Color V3

instance Show Color where
  show (Color (V3 x y z)) =
    let r = 256 * x
        g = 256 * y
        b = 256 * z
     in unwords $ fmap show [r, g, b] -- TODO: More efficient implementation?
