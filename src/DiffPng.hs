module DiffPng where

import CorePrelude
import Prelude (show)
import Algorithms.NaturalSort (sortKey)
import Codec.Picture (convertRGBA8, readPng)
import Codec.Picture.Png (PngSavable, writePng)
import Codec.Picture.Types (Image, PixelRGBA8 (..), generateImage, imageHeight, imageWidth, pixelAt)
import Conduit (filterC, filterMC, sinkList, sourceDirectory)
import Control.Category ((>>>))
import Control.Lens (_head, over)
import Control.Monad (unless)
import Control.Monad.Parallel (forkExec, mapM_)
import Control.Monad.Trans.Resource (MonadResource)
import Data.Bits (complement, shiftR, xor)
import Data.Char (toLower, toUpper)
import Data.Conduit (Producer, (.|), runConduitRes)
import Data.List (length, sortOn, splitAt, zip)
import Data.Text.Lazy (unpack)
import Safe (tailMay)
import System.Directory (copyFile, createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>), takeBaseName, takeDirectory, takeExtension)
import Text.Shakespeare.Text (lt)

--

type Image' = Image PixelRGBA8

readRGBA :: FilePath -> IO Image'
readRGBA path = convertRGBA8 <$> (either error pure =<< readPng path)

pixel :: Image' -> (Int, Int) -> PixelRGBA8
pixel image (i, j)
  | i < w && j < h = (image `pixelAt`) i j
  | otherwise = PixelRGBA8 0 0 0 0
  where
    w = imageWidth image
    h = imageHeight image

diff :: (PixelRGBA8 -> PixelRGBA8 -> PixelRGBA8) -> Image' -> Image' -> Image'
diff mixing p q = generateImage mixed w h
  where
    w = imageWidth p `max` imageWidth q
    h = imageHeight p `max` imageHeight q
    mixed = curry $ mixing <$> pixel p <*> pixel q

mixPreserving :: PixelRGBA8 -> PixelRGBA8 -> PixelRGBA8
mixPreserving (PixelRGBA8 pr pg pb pa) (PixelRGBA8 qr qg qb qa) =
  PixelRGBA8 (pr `xor` qr) (pg `xor` qg) (pb `xor` qb) (complement pa `xor` qa)

mixIndicating :: PixelRGBA8 -> PixelRGBA8 -> PixelRGBA8
mixIndicating a b
  | a == b = darken a
  | otherwise = PixelRGBA8 255 0 0 255
  where
    darken (PixelRGBA8 r g b _) = PixelRGBA8 (r `shiftR` 2) (g `shiftR` 2) (b `shiftR` 2) 255

--

data FileDiff = FileDiff
  { sourceRemainder :: [FilePath]
  , targetRemainder :: [FilePath]
  , diffEntries :: [(FilePath, FilePath)]
  }

filesUnder :: MonadResource m => String -> FilePath -> Producer m FilePath
extension `filesUnder` directory = sourceDirectory directory
  .| filterC ((== Just extension) . tailMay . (toLower <$>) . takeExtension)
  .| filterMC (liftIO . doesFileExist)

candidates :: FilePath -> IO [FilePath]
candidates directory = sortOn sortKey <$> runConduitRes ("png" `filesUnder` directory .| sinkList)

fileDiff :: [FilePath] -> [FilePath] -> FileDiff
fileDiff a b = FileDiff a2 b2 $ a1 `zip` b1
  where
    bb `bisect` aa = splitAt (length bb) aa
    (a1, a2) = b `bisect` a
    (b1, b2) = a `bisect` b

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

--

diffPng :: Bool -> Bool -> (FilePath, FilePath) -> IO ()
diffPng indication noMerged (source, target) = do
  files <- fileDiff <$> candidates source <*> candidates target
  forkExec . unless noMerged . writeMerged $ diffEntries files
  forkExec $ writeLeftovers files
  writeDiffs (bool ("diff", mixPreserving) ("compare", mixIndicating) indication) . diffEntries $ files
