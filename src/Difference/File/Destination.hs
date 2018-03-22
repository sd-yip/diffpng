module Difference.File.Destination where

import Difference (DifferenceT (..))
import Difference.Image.Color (ColorComparison (..))

data Destinations =
  Destinations {
    generated :: ColorComparison -> Maybe FilePath,
    original :: Maybe ColorComparison -> Maybe FilePath
  }

defaultDestinations :: Destinations
defaultDestinations = Destinations generated original
  where
    generated Preservative = Just "diff"
    generated Indicative = Just "compare"
    original (Just _) = Just "merged"
    original Nothing = Just "leftovers"
