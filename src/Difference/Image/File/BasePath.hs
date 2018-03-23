module Difference.Image.File.BasePath where

import Difference (Difference (..))
import Difference.File.BasePath (BaseFilePath)
import Difference.File.OutputType (OutputType)

data DifferenceOptions =
  DifferenceOptions {
    outputPathMay :: OutputType -> Maybe BaseFilePath
  }


instance Difference DifferenceOptions BaseFilePath (IO [(OutputType, FilePath)]) where
  difference _ _ _ = undefined
