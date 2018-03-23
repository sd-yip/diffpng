{-# LANGUAGE FlexibleContexts #-}
module Difference.Image.File.Generation where

import Difference (Difference (..))
import Difference.File.BasePath (BaseFilePath)
import Difference.Image.File (ImageOptions)

generateOutputs :: Difference (ImageOptions a, Int) FilePath (IO FilePath) =>
    Int -> ImageOptions a -> [(FilePath, FilePath)] -> IO [FilePath]
generateOutputs startIndex options inputs = consume `traverse` (inputs `zip` [startIndex..])
  where
    consume ((p, q), i) = difference (options, i) p q
