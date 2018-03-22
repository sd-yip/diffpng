module Difference.File where

import System.FilePath ((</>), takeBaseName)

import Difference (DifferenceI (..))
import Difference.File.Extension (FileExtension (..))

data FileOptions =
  FileOptions {
    directory :: FilePath,
    prefix :: String,
    extension :: FileExtension
  }


instance DifferenceI (FileOptions, Int) FilePath where
  differenceI (FileOptions directory prefix extension, i) p q =
    directory </> prefix ++ show i ++ ' ' : takeBaseName p ++ ' ' : takeBaseName q ++ '.' : name extension
