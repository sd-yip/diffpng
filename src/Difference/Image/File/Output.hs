module Difference.Image.File.Output where

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
