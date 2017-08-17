module DiffPng where

import Algorithms.NaturalSort (sortKey)
import Codec.Picture
import Conduit
import Control.Monad.Except
import Data.Bits (complement, xor)
import Data.List (sortOn)
import Safe (tailMay)
import System.Directory (doesFileExist)
import System.FilePath (takeBaseName, takeExtension)

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

data FileDiff = FileDiff
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

fileDiff :: [FilePath] -> [FilePath] -> FileDiff
fileDiff a b = FileDiff a2 b2 $ a1 `zip` b1
  where
    bb `bisect` aa = splitAt (length bb) aa
    (a1, a2) = b `bisect` a
    (b1, b2) = a `bisect` b

writeDiffs :: [(FilePath, FilePath)] -> ExceptT String IO ()
writeDiffs list = zip [0..] list `forM_` \(i, (a, b)) -> do
  d <- diff <$> readRGBA a <*> readRGBA b
  let path = "diff/Diff" ++ show i ++ " " ++ takeBaseName a ++ " " ++ takeBaseName b ++ ".png"
  liftIO . writePng path $ d

--

diffPng :: FilePath -> FilePath -> IO ()
diffPng source target = do
  files <- fileDiff <$> candidates source <*> candidates target
  result <- runExceptT . writeDiffs . diffEntries $ files
  case result of
    Left e -> error e
    Right _ -> pure ()
