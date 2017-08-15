module DiffPng where

import Algorithms.NaturalSort (sortKey)
import Conduit
import Data.List (sortOn)
import Safe (tailMay)
import System.Directory (doesFileExist)
import System.FilePath (takeExtension)

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
