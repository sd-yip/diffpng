module Difference.Image () where

import Codec.Picture.Types (Image (imageHeight, imageWidth), Pixel (pixelAt), generateImage)

import Difference (Difference (..))
import Difference.Color (Color (..))

pixel :: Color a => Image a -> (Int, Int) -> a
pixel image (i, j)
  | i < w && j < h = (image `pixelAt`) i j
  | otherwise = clearColor
  where
    w = imageWidth image
    h = imageHeight image


instance (Color a, Difference s a) => Difference s (Image a) where
  difference s p q = generateImage diff w h
    where
      w = imageWidth p `max` imageWidth q
      h = imageHeight p `max` imageHeight q
      diff = curry $ difference s <$> pixel p <*> pixel q
