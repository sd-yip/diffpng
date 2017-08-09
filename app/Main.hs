module Main where

import DiffPng
import Options.Applicative

main :: IO ()
main = uncurry diffPng =<< execParser parser
  where
    parser = info
      (args <**> helper)
      fullDesc
    args = (,)
      <$> argument str (metavar "SOURCE_DIRECTORY")
      <*> argument str (metavar "TARGET_DIRECTORY")
