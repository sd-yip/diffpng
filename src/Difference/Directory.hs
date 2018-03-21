module Difference.Directory (
  FileEnumeration (..)
) where

import Conduit (filterC, filterMC)
import Control.Applicative (liftA2)
import Control.Compose ((:.) (..))
import CorePrelude (liftIO)
import Data.Conduit ((.|), runConduitRes)
import Data.Conduit.Combinators (sinkList, sourceDirectory)
import Data.Function (on)
import System.Directory (doesFileExist)

import Difference (DifferenceT (..))
import Difference.File.Extension (FileExtension (..), matchExtension)
import Difference.List (Zipped)

filesUnder :: FileExtension -> FilePath -> IO [FilePath]
extension `filesUnder` directory = runConduitRes $ sourceDirectory directory
  .| filterC (matchExtension extension)
  .| filterMC (liftIO . doesFileExist)
  .| sinkList

data FileEnumeration a =
  FileEnumeration {
    extension :: FileExtension,
    sorting :: Ord a => FilePath -> a
  }


instance Ord a => DifferenceT (FileEnumeration a) (IO :. Zipped :. []) FilePath where
  differenceT options =
    ((O . O) .) . liftA2 (differenceT (sorting options)) `on` (extension options `filesUnder`)
