{-# LANGUAGE TemplateHaskell #-}
module Difference.Directory.Extension (
  FileExtension (name),
  fileExtension,
  matchExtension
) where

import Data.Char (isAsciiLower, toLower)
import Language.Haskell.TH (Exp, Q)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Safe (tailMay)
import System.FilePath (takeExtension)

newtype FileExtension = FileExtension { name :: String }

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
matchExtension extension = any (== name extension) . tailMay . (toLower <$>) . takeExtension
