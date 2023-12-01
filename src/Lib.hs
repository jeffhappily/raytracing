module Lib where

import Conduit
import Data.ByteString (ByteString, toStrict)
import Data.ByteString.Char8 (pack)
import Data.Conduit.Combinators (intersperse)
import Data.Kind (Type)
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Encoding
import System.IO (stdout)
import Vec3 (Color (Color), V3 (V3), toPpm)
import Prelude hiding (concatMap)

calcPixel ::
  forall (n :: Type). (Integral n) => n -> n -> n -> n -> Color
calcPixel width height i j =
  let r = fromIntegral i / (fromIntegral width - 1)
      g = fromIntegral j / (fromIntegral height - 1)
      b = 0.25
   in Color $ V3 r g b

-- Takes an image width and height and returns a string containing the PPM body
content ::
  forall (m :: Type -> Type) (a :: Type).
  (Monad m) =>
  Int ->
  Int ->
  ConduitM a Text m ()
content imgWidth imgHeight =
  let calc = calcPixel imgWidth imgHeight
   in yieldMany
        [ calc i j
        | j <- [imgHeight - 1, imgHeight - 2 .. 0]
        , i <- [0 .. imgWidth - 1]
        ]
        .| mapC toPpm

-- .| mapC pack
-- Takes an image width and height and returns a string containing the PPM
-- header and the image data.
p3 ::
  forall (m :: Type -> Type) (a :: Type).
  (Monad m) =>
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
   in ( yieldMany title
          .| mapC pack
          >> content'
          .| mapC encodeUtf8
          .| mapC toStrict
      )
        .| intersperse "\n"
