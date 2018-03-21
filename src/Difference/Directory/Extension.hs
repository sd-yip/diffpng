{-# LANGUAGE TemplateHaskell #-}
module Difference.Directory.Extension (
  FileExtension (name),
  fileExtension
) where

import Data.Char (isAsciiLower)
import Language.Haskell.TH (Exp, Q)
import Language.Haskell.TH.Quote (QuasiQuoter (..))

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
