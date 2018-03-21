{-# LANGUAGE TypeFamilies #-}
module Difference.List where

import Data.List (sortOn)

import Difference (DifferenceT (..))

data family Zipped m

data instance Zipped [a] =
  ListZipped {
    difference :: Either [a] [a],
    intersection :: [(a, a)]
  }


instance Ord b => DifferenceT (a -> b) Zipped [a] where
  differenceT sorting p q
    | np > nq = unbalanced Left (`zip` q') $ splitAt nq p'
    | otherwise = unbalanced Right (p' `zip`) $ splitAt np q'
    where
      p' = sortOn sorting p
      q' = sortOn sorting q
      np = length p
      nq = length q
      unbalanced side zipping (a1, a2) = ListZipped (side a2) (zipping a1)
