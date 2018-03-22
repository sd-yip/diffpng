module Difference.List where

import Data.List (sortOn)

import Difference (Difference (..))

data Zipped f a =
  Zipped {
    difference :: Either (f a) (f a),
    intersection :: f (a, a)
  }


instance Ord b => Difference (a -> b) [a] (Zipped [] a) where
  difference sorting p q
    | np > nq = unbalanced Left (`zip` q') $ splitAt nq p'
    | otherwise = unbalanced Right (p' `zip`) $ splitAt np q'
    where
      p' = sortOn sorting p
      q' = sortOn sorting q
      np = length p
      nq = length q
      unbalanced side zipping (a1, a2) = Zipped (side a2) (zipping a1)
