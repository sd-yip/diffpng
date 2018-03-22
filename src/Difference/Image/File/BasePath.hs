module Difference.Image.File.BasePath where

import Control.Compose ((:.) (..))

import Difference (DifferenceT (..))
import Difference.File.Destination (OutputType)

instance DifferenceT () ([] :. (,) OutputType) FilePath where
  differenceT _ _ _ = O undefined
