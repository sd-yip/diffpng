{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Difference where

import Control.Applicative (liftA2)
import Control.Monad.Identity (Identity)

data Strategy = Preservative | Indicative


class DifferenceT f g a where
  differenceT :: Strategy -> f a -> f a -> g a

class Difference a where
  difference :: Strategy -> a -> a -> a

instance Difference a => DifferenceT Identity Identity a where
  differenceT = liftA2 . difference
