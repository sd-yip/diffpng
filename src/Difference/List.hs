{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Difference.List where

import Difference (DifferenceT (..))

data ListCollation a = ListCollation {
  difference :: ([a], [a]),
  intersection :: [(a, a)]
}


instance DifferenceT [] ListCollation a where
  differenceT _ p q = ListCollation (p2, q2) $ p1 `zip` q1
    where
      a `bisect` b = splitAt (length b) a
      (p1, p2) = q `bisect` p
      (q1, q2) = p `bisect` q
