module Ray where

import Vec3 (Point3, V3)

data Ray = Ray
  { origin :: Point3
  , direction :: V3
  }

-- rayAt :: Ray -> Double -> Point3
-- rayAt r t = origin r + t * direction r
