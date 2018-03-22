module Difference.Image () where

import Codec.Picture.Types (Image (imageHeight, imageWidth), Pixel (pixelAt), generateImage)

import Difference (DifferenceI (..), difference)
import Difference.Image.Color (Color (..))

pixel :: Color a => Image a -> (Int, Int) -> a
pixel image (i, j)
  | i < w && j < h = (image `pixelAt`) i j
  | otherwise = clearColor
  where
    w = imageWidth image
    h = imageHeight image


instance (Color a, DifferenceI s a) => DifferenceI s (Image a) where
  differenceI s p q = generateImage diff w h
    where
      w = imageWidth p `max` imageWidth q
      h = imageHeight p `max` imageHeight q
      diff = curry $ difference s <$> pixel p <*> pixel q
