{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Difference where

import Control.Applicative (liftA2)
import Data.Functor.Identity (Identity)

class DifferenceT s f g a where
  differenceT :: s -> f a -> f a -> g a

class Difference s a where
  difference :: s -> a -> a -> a

instance Difference s a => DifferenceT s Identity Identity a where
  differenceT = liftA2 . difference
