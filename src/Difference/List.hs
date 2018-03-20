{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Difference.List where

import Difference (DifferenceT (..))

data SaturatedZip a = SaturatedZip {
  difference :: Either [a] [a],
  intersection :: [(a, a)]
}


instance DifferenceT [] SaturatedZip a where
  differenceT _ p q
    | np > nq = unbalanced Left (`zip` q) $ splitAt nq p
    | otherwise = unbalanced Right (p `zip`) $ splitAt np q
    where
      np = length p
      nq = length q
      unbalanced side zipping (a1, a2) = SaturatedZip (side a2) (zipping a1)
