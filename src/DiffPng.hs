module DiffPng where

import Prelude (show)
import Algorithms.NaturalSort (sortKey)
import Codec.Picture
import Conduit
import Control.Category ((>>>))
import Control.Lens (_head, over)
import Control.Monad (unless)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.Parallel (mapM_)
import CorePrelude
import Data.Bits (complement, shiftR, xor)
import Data.Char (toLower, toUpper)
import Data.List (length, sortOn, splitAt, zip)
import Data.Text.Lazy (unpack)
import Safe (tailMay)
import System.Directory (copyFile, createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>), takeBaseName, takeDirectory, takeExtension)
import Text.Shakespeare.Text (lt)

apply = ($)

--

type Image' = Image PixelRGBA8

readRGBA :: FilePath -> ExceptT String IO Image'
readRGBA path = convertRGBA8 <$> ExceptT (readPng path)

pixel :: Image' -> (Int, Int) -> PixelRGBA8
pixel image (i, j)
  | i < w && j < h = (image `pixelAt`) i j
  | otherwise = PixelRGBA8 0 0 0 0
  where
    w = imageWidth image
    h = imageHeight image

diff :: (PixelRGBA8 -> PixelRGBA8 -> PixelRGBA8) -> Image' -> Image' -> Image'
diff mixing p q = generateImage mixAt w h
  where
    w = imageWidth p `max` imageWidth q
    h = imageHeight p `max` imageHeight q
    mixAt = curry $ mixing <$> pixel p <*> pixel q

mixPreserving :: PixelRGBA8 -> PixelRGBA8 -> PixelRGBA8
mixPreserving (PixelRGBA8 pr pg pb pa) (PixelRGBA8 qr qg qb qa) =
  PixelRGBA8 (pr `xor` qr) (pg `xor` qg) (pb `xor` qb) (complement pa `xor` qa)

mixIndicating :: PixelRGBA8 -> PixelRGBA8 -> PixelRGBA8
mixIndicating a b
  | a == b = lighten a
  | otherwise = PixelRGBA8 255 0 0 255
  where
    lighten (PixelRGBA8 r g b _) = PixelRGBA8 (r `shiftR` 2) (g `shiftR` 2) (b `shiftR` 2) 255

--

data FileDiff = FileDiff
  { sourceRemainder :: [FilePath]
  , targetRemainder :: [FilePath]
  , diffEntries :: [(FilePath, FilePath)]
  }
  deriving Show

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

createParentDirectories :: FilePath -> IO ()
createParentDirectories = createDirectoryIfMissing True . takeDirectory

writePng' :: PngSavable p => FilePath -> Image p -> IO ()
writePng' path pixels = createParentDirectories path *> writePng path pixels

copyFile' :: FilePath -> FilePath -> IO ()
copyFile' source target = createParentDirectories target *> copyFile source target

writeDiffs :: (String, (PixelRGBA8 -> PixelRGBA8 -> PixelRGBA8)) -> [(FilePath, FilePath)] -> ExceptT String IO ()
writeDiffs (prefix, mixing) = zip [0..] >>> mapM_ `apply` \(i, (a, b)) -> do
  pixels <- diff mixing <$> readRGBA a <*> readRGBA b
  let path = prefix </> unpack [lt|#{over _head toUpper prefix}#{show i} #{takeBaseName a} #{takeBaseName b}.png|]
  liftIO . writePng' path $ pixels

writeCopy :: FilePath -> Int -> String -> FilePath -> IO ()
writeCopy directory i code path = copyFile' path $ directory </> unpack [lt|#{show i}#{code} #{takeBaseName path}.png|]

writeMerged :: [(FilePath, FilePath)] -> IO ()
writeMerged = zip [0..] >>> mapM_ `apply` \(i, (a, b)) -> (write i "a" a *> write i "b" b)
  where
    write = writeCopy "merged"

writeLeftovers :: FileDiff -> IO ()
writeLeftovers (FileDiff r1 r2 e) = out "a" r1 *> out "b" r2
  where
    out code = zip [length e..] >>> mapM_ `apply` \(i, path) -> writeCopy "leftovers" i code path

--

diffPng :: Bool -> Bool -> (FilePath, FilePath) -> IO ()
diffPng indication noMerged (source, target) = do
  files <- fileDiff <$> candidates source <*> candidates target
  _ <- unless noMerged . writeMerged . diffEntries $ files
  _ <- writeLeftovers files
  result <- runExceptT . out . diffEntries $ files
  either error pure result
  where
    out = writeDiffs (bool ("diff", mixPreserving) ("compare", mixIndicating) indication)
