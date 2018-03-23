{-# LANGUAGE TemplateHaskell #-}
module Difference.File.Extension (
  FileExtension,
  fileExtension,
  matchExtension
) where

import Data.Char (isAsciiLower, toLower)
import Language.Haskell.TH (Exp, Q)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Safe (tailMay)
import System.FilePath (takeExtension)

newtype FileExtension = FileExtension String

fileExtension :: QuasiQuoter
fileExtension =
  QuasiQuoter {
    quotePat = undefined, quoteType = undefined, quoteDec = undefined,
    quoteExp = \case
      name
        | all isAsciiLower name -> [|FileExtension name|]
        | otherwise -> fail "Lower ASCII only"
  }

matchExtension :: FileExtension -> FilePath -> Bool
matchExtension extension = any (== show extension) . tailMay . (toLower <$>) . takeExtension


instance Show FileExtension where
  show (FileExtension name) = name
