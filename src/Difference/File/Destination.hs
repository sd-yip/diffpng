module Difference.File.Destination where

import Difference (DifferenceT (..))
import Difference.Image.Color (ColorComparison (..))

data OutputType = Generated ColorComparison | Original (Maybe ColorComparison)

defaultPath :: OutputType -> FilePath
defaultPath (Generated Preservative) = "diff"
defaultPath (Generated Indicative) = "compare"
defaultPath (Original (Just _)) = "merged"
defaultPath (Original Nothing) = "leftovers"
