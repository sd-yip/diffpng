module Difference.Image.File.Generation where

import Difference (Difference (..))
import Difference.File.BasePath (BaseFilePath)
import Difference.Image.Color (ColorComparison)

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
-}

data GenerationOptions =
  GenerationOptions {
    colorComparison :: ColorComparison,
    outputPath :: BaseFilePath
  }


instance Difference GenerationOptions [FilePath] (IO [FilePath]) where
  difference _ _ _ = undefined
