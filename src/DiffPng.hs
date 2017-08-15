module DiffPng where

import Conduit
import Safe
import Algorithms.NaturalSort
import System.FilePath (takeExtension)
import System.Directory (doesFileExist)
import Data.List (sortOn)

filesByExtension :: MonadResource m => String -> FilePath -> Producer m FilePath
filesByExtension extension directory = sourceDirectory directory
  .| filterC ((== Just extension) . tailMay . takeExtension)
  .| filterMC (liftIO . doesFileExist)

printEntries :: String -> IO ()
printEntries directory = print =<< sortOn sortKey <$> entries
  where
    entries = runConduitRes (filesByExtension "png" directory .| sinkList)

diffPng :: FilePath -> FilePath -> IO ()
diffPng source target = printEntries source >> printEntries target
