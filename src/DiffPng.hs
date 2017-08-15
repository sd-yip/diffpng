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

candidates :: FilePath -> IO [FilePath]
candidates directory = sortOn sortKey <$> runConduitRes (filesByExtension "png" directory .| sinkList)

printEntries :: FilePath -> IO ()
printEntries directory = print =<< candidates directory

diffPng :: FilePath -> FilePath -> IO ()
diffPng source target = printEntries source >> printEntries target
