module Difference.Color where

import Codec.Picture.Types (Image, PixelRGBA8 (..))
import Data.Bits (complement, shiftR, xor)
import Difference (Difference (..), Strategy (..))

instance Difference PixelRGBA8 where
  -- XOR difference of pixels
  difference Preservative (PixelRGBA8 pr pg pb pa) (PixelRGBA8 qr qg qb qa) =
    PixelRGBA8 (pr `xor` qr) (pg `xor` qg) (pb `xor` qb) (complement pa `xor` qa)

  -- Mark as red for any differences
  difference Indicative a b
    | a == b = darken a
    | otherwise = PixelRGBA8 255 0 0 255
    where
      darken (PixelRGBA8 r g b _) = PixelRGBA8 (r `shiftR` 2) (g `shiftR` 2) (b `shiftR` 2) 255
