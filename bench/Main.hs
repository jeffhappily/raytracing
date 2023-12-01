-- Using tasty-bench, compare the difference between modifyIORef and atomicModifyIORef

import Conduit
import Control.Concurrent
import Control.Monad
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.Conduit.Combinators (intersperse)
import Data.Kind (Type)
import Data.Traversable (for)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Lib

-- import Streamly.Data.Stream.Prelude qualified as S
import System.IO (stdout)
import Test.Tasty.Bench
import Vec3 (Color (Color), V3 (V3), toPpm)
import Prelude hiding (concatMap)

import Streamly.Internal.Data.Stream.IsStream qualified as S

-- import Streamly (asyncly, parallely, wAsyncly)

-- import Streamly.Prelude qualified as S

import Data.Function ((&))
import Data.Text

-- import Streamly (asyncly)

content :: (MonadAsync m) => Int -> Int -> Stream m Text
content imgWidth imgHeight =
  S.fromList [(i, j) | j <- [imgHeight - 1, imgHeight - 2 .. 0], i <- [0 .. imgWidth - 1]]
    & parMapM (maxThreads 10) (\(i, j) -> return $ calcPixel i j imgWidth imgHeight >>= return . toPpm)

main :: IO ()
main =
  defaultMain
    [ bgroup
        "Image Generation"
        [ bench "Conduit" $ nfIO $ benchmarkP3 1000 1000 "bench.ppm"
        ]
    ]

benchmarkP3 :: Int -> Int -> FilePath -> IO ()
benchmarkP3 width height tempFilePath =
  runConduitRes
    $ p3 width height
    .| sinkFileBS tempFilePath

-- .| mapC pack

-- contentVector :: Int -> Int -> Vector String
-- contentVector imgWidth imgHeight =
--   V.map toPpm
--     $ V.generate
--       (imgWidth * imgHeight)
--       ( \index ->
--           calcPixel
--             (index `mod` imgWidth)
--             (index `div` imgWidth)
--             imgWidth
--             imgHeight
--       )
