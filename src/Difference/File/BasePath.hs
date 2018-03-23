module Difference.File.BasePath (
  BaseFilePath (..),
  FileEnumeration (..)
) where

import Conduit (filterC, filterMC)
import Control.Applicative (liftA2)
import Control.Monad.IO.Class (liftIO)
import Data.Conduit ((.|), runConduitRes)
import Data.Conduit.Combinators (sinkList, sourceDirectory)
import Data.Function (on)
import System.Directory (doesFileExist)

import Difference (Difference (..))
import Difference.File.Extension (FileExtension (..), matchExtension)
import Difference.List (Zipped)

filesUnder :: FileExtension -> FilePath -> IO [FilePath]
extension `filesUnder` directory = runConduitRes $ sourceDirectory directory
    .| filterC (matchExtension extension)
    .| filterMC (liftIO . doesFileExist)
    .| sinkList

newtype BaseFilePath = BaseFilePath { unBaseFilePath :: FilePath }

data FileEnumeration a =
  FileEnumeration {
    extension :: FileExtension,
    sorting :: FilePath -> a
  }


instance Ord a => Difference (FileEnumeration a) BaseFilePath (IO (Zipped [] FilePath)) where
  difference (FileEnumeration extension sorting) =
    liftA2 (difference sorting) `on` (extension `filesUnder`) . unBaseFilePath
