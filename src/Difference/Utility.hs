module Difference.Utility where

(*<) :: Applicative f => f a -> f b -> f a
(*<) = flip (*>)
