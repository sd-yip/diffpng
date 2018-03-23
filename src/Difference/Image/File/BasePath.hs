module Difference.Image.File.BasePath where

import Difference (Difference (..))
import Difference.File.BasePath (BaseFilePath)
import Difference.File.Output (OutputType)

data OutputOptions =
  OutputOptions {
    outputPath :: OutputType -> Maybe BaseFilePath
  }


instance Difference OutputOptions BaseFilePath (IO [(OutputType, FilePath)]) where
  difference _ _ _ = undefined
