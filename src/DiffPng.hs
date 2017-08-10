module DiffPng where

import Conduit
import Safe
import System.FilePath (takeExtension)
import System.Directory (doesFileExist)

filesByExtension :: MonadResource m => String -> FilePath -> Producer m FilePath
filesByExtension extension directory = sourceDirectory directory
  .| filterC ((== Just extension) . tailMay . takeExtension)
  .| filterMC (liftIO . doesFileExist)

diffPng :: FilePath -> FilePath -> IO ()
diffPng source target = do
  runConduitRes $ entries source
  runConduitRes $ entries target
  where
    entries directory = filesByExtension "png" directory .| mapM_C (liftIO . putStrLn)
