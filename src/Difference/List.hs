module Difference.List where

import Data.Function (on)
import Data.List (sortOn)

import Difference (Difference (..))

data Zipped f a =
  Zipped {
    difference :: Either (f a) (f a),
    intersection :: f (a, a)
  }


instance Ord b => Difference (a -> b) [a] (Zipped [] a) where
  difference = (zipped `on`) . sortOn
    where
      make zipRest sourceSide (prefix, suffix) = sourceSide suffix `Zipped` zipRest prefix
      zipped p q
        | np > nq = make (`zip` q) Left $ splitAt nq p
        | otherwise = make (p `zip`) Right $ splitAt np q
        where
          np = length p
          nq = length q
