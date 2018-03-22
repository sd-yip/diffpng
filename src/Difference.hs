module Difference where

import Data.Functor.Identity (Identity (..))

class DifferenceT s f a where
  differenceT :: s -> a -> a -> f a

class Difference s a where
  difference :: s -> a -> a -> a

instance Difference s a => DifferenceT s Identity a where
  differenceT = ((pure .) .) . difference
