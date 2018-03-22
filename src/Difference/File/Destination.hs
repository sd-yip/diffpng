module Difference.File.Destination where

import Difference (DifferenceT (..))
import Difference.Image.Color (ColorComparison (..))

data Destinations =
  Destinations {
    differences :: ColorComparison -> Maybe FilePath,
    originals :: Maybe ColorComparison -> Maybe FilePath
  }

defaultDestinations :: Destinations
defaultDestinations = Destinations differences originals
  where
    differences Preservative = Just "diff"
    differences Indicative = Just "compare"
    originals (Just _) = Just "merged"
    originals Nothing = Just "leftovers"
