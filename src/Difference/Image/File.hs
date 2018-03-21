{-# LANGUAGE FlexibleContexts #-}
module Difference.Image.File where

import Codec.Picture.Types (Image)
import Data.Functor.Identity (Identity)
import System.FilePath ((</>), takeBaseName)

import Difference (Difference (..), DifferenceT (..))
import Difference.Directory.Extension (FileExtension (..))
import Difference.Image.Color (ColorComparison)

data FileNameOptions =
  FileNameOptions {
    directory :: FilePath,
    prefix :: String,
    extension :: FileExtension
  }

data ImageOptions a =
  ImageOptions {
    readImage :: FilePath -> IO (Image a),
    writeImage :: FilePath -> Image a -> IO (),
    colorComparison :: ColorComparison
  }


instance Difference (FileNameOptions, Int) FilePath where
  difference (FileNameOptions directory prefix extension, i) p q =
    directory </> prefix ++ show i ++ ' ' : takeBaseName p ++ ' ' : takeBaseName q ++ name extension

instance Difference ColorComparison (Image a) => DifferenceT (ImageOptions a) IO FilePath where
  differenceT = undefined -- TODO
