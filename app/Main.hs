module Main where

import Data.Maybe
import DiffPng
import Options.Applicative

main :: IO ()
main = uncurry diffPng . fromMaybe ("input0", "input1") =<< execParser parser
  where
    parser = info
      (optional args <**> helper)
      fullDesc
    args = (,)
      <$> argument str (metavar "SOURCE_DIRECTORY")
      <*> argument str (metavar "TARGET_DIRECTORY")
