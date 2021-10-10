module Main where

import Conduit
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.Conduit.Combinators (intersperse)
import Data.Kind (Type)
import Prelude hiding (concatMap)

calcPixel ::
  forall (n :: Type). (Integral n) => n -> n -> n -> n -> (n, n, n)
calcPixel i j width height =
  let r = fromIntegral i / (fromIntegral width - 1)
      g = fromIntegral j / (fromIntegral height - 1)
      b = 0.25
      rgb = 255.999
      round' = round . (*) rgb
   in (round' r, round' g, round' b)

content ::
  forall (m :: Type -> Type) (a :: Type).
  Monad m =>
  Int ->
  Int ->
  ConduitM a ByteString m ()
content imgWidth imgHeight =
  yieldMany
    [ calcPixel i j imgWidth imgHeight
    | j <- [imgHeight -1, imgHeight -2 .. 0]
    , i <- [0 .. imgWidth -1]
    ]
    .| mapC toLine
    .| mapC pack
  where
    toLine :: (Int, Int, Int) -> String
    toLine (ir, ig, ib) = unwords (fmap show [ir, ig, ib])

p3 ::
  forall (m :: Type -> Type) (a :: Type).
  Monad m =>
  Int ->
  Int ->
  ConduitM a ByteString m ()
p3 imgWidth imgHeight =
  let title =
        [ "P3"
        , show imgWidth <> " " <> show imgHeight
        , "255"
        ]
      content' = content imgWidth imgHeight
   in ( yieldMany title .| mapC pack
          >> content'
      )
        .| intersperse "\n"

main :: IO ()
main = do
  let imgWidth = 256
  let imgHeight = 256
  runConduitRes $
    p3 imgWidth imgHeight
      .| sinkFileBS "test.ppm"
