module DiffPng where

diffPng :: Bool -> Bool -> (FilePath, FilePath) -> IO ()
diffPng indicative noMerged (source, target) = undefined {-do
  files <- fileDiff <$> candidates source <*> candidates target
  forkExec . unless noMerged . writeMerged $ diffEntries files
  forkExec $ writeLeftovers files
  writeDiffs (bool ("diff", preservativeMixing) ("compare", indicativeMixing) indicative) $ diffEntries files-}
