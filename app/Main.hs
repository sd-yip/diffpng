module Main where

import Control.Monad (join)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import DiffPng (diffPng)
import Options.Applicative

main :: IO ()
main = join . execParser $ info (options <**> helper) fullDesc
  where
    options = (\i -> diffPng i i)
      <$> switch (short 'I' <> long "indicative" <> help "Indicate differences onto each source image")
      <*> (fromMaybe ("input0", "input1") <$> optional ((,)
        <$> argument str (metavar "SOURCE_DIRECTORY")
        <*> argument str (metavar "TARGET_DIRECTORY")))
