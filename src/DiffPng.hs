module DiffPng where

import Algorithms.NaturalSort (sortKey)
import Conduit
import Data.List (sortOn)
import Safe (tailMay)
import System.Directory (doesFileExist)
import System.FilePath (takeExtension)

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

diffPng :: FilePath -> FilePath -> IO ()
diffPng source target = print =<< diffSet <$> candidates source <*> candidates target
