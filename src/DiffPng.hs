module DiffPng where

import Algorithms.NaturalSort (sortKey)
import Codec.Picture
import Conduit
import Control.Monad.Except
import Data.Bits (complement, xor)
import Data.List (sortOn)
import Safe (tailMay)
import System.Directory (doesFileExist)
import System.FilePath (takeExtension)

--

type Image' = Image PixelRGBA8

readRGBA :: FilePath -> ExceptT String IO Image'
readRGBA path = convertRGBA8 <$> ExceptT (readPng path)

pixel :: Image' -> (Int, Int) -> PixelRGBA8
pixel image (i, j)
  | i < w && j < h = pixelAt image i j
  | otherwise = PixelRGBA8 0 0 0 0
  where
    w = imageWidth image
    h = imageHeight image

diff :: Image' -> Image' -> Image'
diff p q = generateImage mixAt w h
  where
    w = imageWidth  p `max` imageWidth  q
    h = imageHeight p `max` imageHeight q
    mix (PixelRGBA8 pr pg pb pa) (PixelRGBA8 qr qg qb qa) =
      PixelRGBA8 (pr `xor` qr) (pg `xor` qg) (pb `xor` qb) (complement pa `xor` qa)
    mixAt = curry $ mix <$> pixel p <*> pixel q

--

data DiffSet = DiffSet
  { sourceRemainder :: [FilePath]
  , targetRemainder :: [FilePath]
  , diffEntries :: [(FilePath, FilePath)]
  }
  deriving Show

filesByExtension :: MonadResource m => String -> FilePath -> Producer m FilePath
filesByExtension extension directory = sourceDirectory directory
  .| filterC ((== Just extension) . tailMay . takeExtension)
  .| filterMC (liftIO . doesFileExist)

candidates :: FilePath -> IO [FilePath]
candidates directory = sortOn sortKey <$> runConduitRes (filesByExtension "png" directory .| sinkList)

diffSet :: [FilePath] -> [FilePath] -> DiffSet
diffSet a b = DiffSet a2 b2 $ a1 `zip` b1
  where
    bb `bisect` aa = splitAt (length bb) aa
    (a1, a2) = b `bisect` a
    (b1, b2) = a `bisect` b

--

diffPng :: FilePath -> FilePath -> IO ()
diffPng source target = print =<< diffSet <$> candidates source <*> candidates target
