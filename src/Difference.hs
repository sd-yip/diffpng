{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Difference where

import Control.Monad.Identity (Identity)

data Strategy = Preservative | Indicative

class DifferenceT f a where
  differenceT :: Strategy -> a -> a -> f a

class Difference a where
  difference :: Strategy -> a -> a -> a

instance Difference a => DifferenceT Identity a where
  differenceT = ((pure .) .) . difference
