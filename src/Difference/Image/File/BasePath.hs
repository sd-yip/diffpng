module Difference.Image.File.BasePath where

import Difference (Difference (..))
import Difference.File.OutputType (OutputType)

data DifferenceOptions =
  DifferenceOptions {
    outputPathMay :: OutputType -> Maybe String
  }


instance Difference DifferenceOptions FilePath (IO [(OutputType, FilePath)]) where
  difference _ _ _ = undefined
