{-# LANGUAGE FlexibleContexts #-}
module Difference.Image.File where

import Codec.Picture.Types (Image)
import Control.Applicative (liftA2)
import Data.Functor.Identity (Identity)
import Data.Function (on)
import System.FilePath ((</>), takeBaseName)

import Difference (Difference (..), DifferenceT (..))
import Difference.Directory.Extension (FileExtension (..))
import Difference.Image.Color (ColorComparison)

data FileOptions =
  FileOptions {
    directory :: FilePath,
    prefix :: String,
    extension :: FileExtension
  }

data ImageOptions a =
  ImageOptions {
    readImage :: FilePath -> IO (Image a),
    writeImage :: FilePath -> Image a -> IO (),
    colorComparison :: ColorComparison,
    fileOptions :: FileOptions
  }


instance Difference (FileOptions, Int) FilePath where
  difference (FileOptions directory prefix extension, i) p q =
    directory </> prefix ++ show i ++ ' ' : takeBaseName p ++ ' ' : takeBaseName q ++ '.' : name extension

instance Difference ColorComparison (Image a) => DifferenceT (ImageOptions a, Int) IO FilePath where
  differenceT (ImageOptions read write c f, i) p q = write' (difference (f, i) p q) =<< p `diff` q
    where
      diff = liftA2 (difference c) `on` read
      write' file = (file <$) . write file
