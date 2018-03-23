{-# LANGUAGE FlexibleContexts #-}
module Difference.Image.File.Output (
  writeGenerated,
  writeOriginal
) where

import qualified Control.Monad.Parallel as P
import System.Directory (copyFile)
import System.FilePath ((</>), takeBaseName, takeDirectory)

import Difference (Difference (..))
import Difference.File (FileOptions (..), createParentDirectories)
import Difference.File.BasePath (BaseFilePath (..))
import Difference.Image.File (ImageOptions)

copyFile' :: FilePath -> FilePath -> IO FilePath
copyFile' source target = target <$ copyFile source target <* createParentDirectories target


writeGenerated :: Difference (ImageOptions a, Int) FilePath (IO FilePath) =>
    Int -> ImageOptions a -> [(FilePath, FilePath)] -> IO [FilePath]
writeGenerated startIndex options inputs = consume `P.mapM` (inputs `zip` [startIndex ..])
  where
    consume ((p, q), i) = difference (options, i) p q

writeOriginal :: Int -> (FileOptions, String) -> [FilePath] -> IO [FilePath]
writeOriginal startIndex (FileOptions directory prefix extension, code) inputs =
  consume `traverse` (inputs `zip` [startIndex ..])
    where
      consume (originalFile, i) = copyFile' originalFile $ unBaseFilePath directory
          </> show i ++ code ++ ' ' : takeBaseName originalFile ++ '.' : show extension
