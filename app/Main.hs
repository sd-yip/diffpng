module Main where

import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import DiffPng (diffPng)
import Options.Applicative

main :: IO ()
main = id =<< execParser parser
  where
    parser = info
      (options <**> helper)
      fullDesc
    options = diffPng'
      <$> switch (short 'I' <> long "indicate" <> help "Indicate differences onto each source image")
      <*> (optional $ (,)
        <$> argument str (metavar "SOURCE_DIRECTORY")
        <*> argument str (metavar "TARGET_DIRECTORY"))

diffPng' :: Bool -> Maybe (String, String) -> IO ()
diffPng' i = diffPng i i . fromMaybe ("input0", "input1")
