module Difference.File where

import System.FilePath ((</>), takeBaseName)

import Difference (DifferenceI (..))
import Difference.File.BasePath (BaseFilePath (..))
import Difference.File.Extension (FileExtension)

data FileOptions =
  FileOptions {
    directory :: BaseFilePath,
    prefix :: String,
    extension :: FileExtension
  }


instance DifferenceI (FileOptions, Int) FilePath where
  differenceI (FileOptions directory prefix extension, i) p q = unBaseFilePath directory
      </> prefix ++ show i ++ ' ' : takeBaseName p ++ ' ' : takeBaseName q ++ '.' : show extension
