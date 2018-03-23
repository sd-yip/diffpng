module Difference.Image.File.OutputType where

import Difference (Difference (..))
import Difference.File.OutputType (OutputType)

{-
consumeIndexed :: Int -> ((Int, a) -> IO ()) -> [a] -> IO ()
consumeIndexed initialIndex consumer = (consumer `mapM_`) . zip [initialIndex..]

createParentDirectories :: FilePath -> IO ()
createParentDirectories = takeDirectory >>> createDirectoryIfMissing True -- Recursively

writeDiffs :: (String, PixelRGBA8 -> PixelRGBA8 -> PixelRGBA8) -> [(FilePath, FilePath)] -> IO ()
writeDiffs (prefix, mixing) = consumeIndexed 0 $ \(i, (a, b)) -> do
  pixels <- diff mixing <$> readRGBA a <*> readRGBA b
  let targetPath = prefix </> unpack [lt|#{over _head toUpper prefix}#{show i} #{takeBaseName a} #{takeBaseName b}.png|]
  createParentDirectories targetPath *> writePng targetPath pixels

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

data OutputOptions =
  OutputOptions {
    outputType :: OutputType,
    outputPath :: String
  }


instance Difference OutputOptions FilePath (IO [FilePath]) where
  difference _ _ _ = undefined
