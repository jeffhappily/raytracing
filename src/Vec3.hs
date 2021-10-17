module Vec3 (V3 (V3)) where

data V3
  = V3
      {-# UNPACK #-} !Double
      {-# UNPACK #-} !Double
      {-# UNPACK #-} !Double
  deriving (Show, Eq, Ord)
