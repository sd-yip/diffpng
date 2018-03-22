module Difference.Image.File.BasePath where

import Difference (Difference (..))
import Difference.File.Destination (OutputType)

instance Difference () FilePath [(OutputType, FilePath)] where
  difference _ _ _ = undefined
