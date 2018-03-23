module Difference.File.OutputType where

import Difference.File.BasePath (BaseFilePath (..))
import Difference.Image.Color (ColorComparison (..))

data OutputType = Generated ColorComparison | Original (Maybe ColorComparison)

defaultPath :: OutputType -> BaseFilePath
defaultPath t = BaseFilePath $ case t of
  Generated Preservative -> "diff"
  Generated Indicative -> "compare"
  Original (Just _) -> "merged"
  Original Nothing -> "leftovers"
