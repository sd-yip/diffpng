module DiffPng where

diffPng :: FilePath -> FilePath -> IO ()
diffPng source target = do
  putStrLn source
  putStrLn target
