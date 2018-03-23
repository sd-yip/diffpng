{-# LANGUAGE FlexibleContexts #-}
module Difference.Image.File.Output where

import Difference (Difference (..))
import Difference.File.BasePath (BaseFilePath)
import Difference.Image.File (ImageOptions)

{-
writePlainCopy :: FilePath -> Int -> String -> FilePath -> IO ()
writePlainCopy targetDirectory i code sourcePath =
  copyFile' sourcePath $ targetDirectory </> unpack [lt|#{show i}#{code} #{takeBaseName sourcePath}.png|]
  where
    copyFile' source target = createParentDirectories target *> copyFile source target

writeMerged :: [(FilePath, FilePath)] -> IO ()
writeMerged = consumeIndexed 0 $ \(i, (a, b)) -> writeCopy i "a" a *> writeCopy i "b" b
  where
    writeCopy = writePlainCopy "merged"

writeLeftovers :: FileDiff -> IO ()
writeLeftovers (FileDiff ra rb e) = writeCopies "a" ra *> writeCopies "b" rb
  where
    writeCopies code = consumeIndexed (length e) $ \(i, path) -> writePlainCopy "leftovers" i code path
-}

writeGenerated :: Difference (ImageOptions a, Int) FilePath (IO FilePath) =>
    Int -> ImageOptions a -> [(FilePath, FilePath)] -> IO [FilePath]
writeGenerated startIndex options inputs = consume `traverse` (inputs `zip` [startIndex..])
  where
    consume ((p, q), i) = difference (options, i) p q
