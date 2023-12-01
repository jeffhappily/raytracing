module Main where

-- import Conduit
-- import Data.ByteString (ByteString)
-- import Data.ByteString.Char8 (pack)
-- import Data.Conduit.Combinators (intersperse)
-- import Data.Kind (Type)
import System.IO (stdout)

-- import Vec3 (Color (Color), V3 (V3), toPpm)
-- import Prelude hiding (concatMap)

import Conduit
import Lib

main :: IO ()
main = do
  let imgWidth = 256
  let imgHeight = 256
  runConduitRes
    $ p3 imgWidth imgHeight
    .| sinkHandle stdout

-- .| sinkFileBS "test.ppm"
