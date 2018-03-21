module Difference.Directory where

import Conduit (filterC, filterMC, sinkList, sourceDirectory)
import Control.Applicative (liftA2)
import Control.Compose ((:.) (..))
import CorePrelude (liftIO)
import Data.Char (toLower)
import Data.Conduit ((.|), runConduitRes)
import Data.Function (on)
import Data.Functor.Identity (Identity, runIdentity)
import Safe (tailMay)
import System.Directory (doesFileExist)
import System.FilePath (takeExtension)

import Difference (DifferenceT (..))
import Difference.Directory.Extension (FileExtension (..))
import Difference.List (SaturatedZip)

filesUnder :: FileExtension -> FilePath -> IO [FilePath]
extension `filesUnder` directory = runConduitRes $ sourceDirectory directory
  .| filterC (any (== name extension) . tailMay . (toLower <$>) . takeExtension)
  .| filterMC (liftIO . doesFileExist)
  .| sinkList

data FileEnumeration a =
  FileEnumeration {
    extension :: FileExtension,
    sorting :: Ord a => FilePath -> a
  }


instance Ord a => DifferenceT (FileEnumeration a) Identity (IO :. SaturatedZip) FilePath where
  differenceT options =
    (O .) . liftA2 (differenceT (sorting options)) `on` (extension options `filesUnder`) . runIdentity
