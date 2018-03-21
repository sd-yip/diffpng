module Difference.File where

import System.FilePath ((</>), takeBaseName)

import Difference (Difference (..))
import Difference.File.Extension (FileExtension (..))

data FileOptions =
  FileOptions {
    directory :: FilePath,
    prefix :: String,
    extension :: FileExtension
  }


instance Difference (FileOptions, Int) FilePath where
  difference (FileOptions directory prefix extension, i) p q =
    directory </> prefix ++ show i ++ ' ' : takeBaseName p ++ ' ' : takeBaseName q ++ '.' : name extension
