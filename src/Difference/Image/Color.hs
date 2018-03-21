module Difference.Image.Color where

import Codec.Picture.Types (Image, Pixel, PixelRGBA8 (..))
import Data.Bits (complement, shiftR, xor)

import Difference (Difference (..))

data ColorComparison = Preservative | Indicative


class Pixel a => Color a where
  clearColor :: a

instance Color PixelRGBA8 where
  clearColor = PixelRGBA8 0 0 0 0


instance Difference ColorComparison PixelRGBA8 where
  -- Simple XOR difference
  difference Preservative (PixelRGBA8 pr pg pb pa) (PixelRGBA8 qr qg qb qa) =
    PixelRGBA8 (pr `xor` qr) (pg `xor` qg) (pb `xor` qb) (complement pa `xor` qa)

  -- Mark as red for any unequal bits
  difference Indicative p q
    | p == q = darken p
    | otherwise = PixelRGBA8 255 0 0 255
    where
      darken (PixelRGBA8 r g b _) = PixelRGBA8 (r `shiftR` 2) (g `shiftR` 2) (b `shiftR` 2) 255
