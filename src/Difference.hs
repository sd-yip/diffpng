module Difference where

class Difference s a b where
  difference :: s -> a -> a -> b

class DifferenceI s a where
  differenceI :: s -> a -> a -> a

instance DifferenceI s a => Difference s a a where
  difference = differenceI
