{-# LANGUAGE LambdaCase, TemplateHaskell, QuasiQuotes #-}
module Difference.Directory (
  FileExtension (name),
  fileExtension
) where

import Data.Char (isAsciiLower)
import Difference (DifferenceT (..))
import Language.Haskell.TH (Exp, Q)
import Language.Haskell.TH.Quote (QuasiQuoter (..))

newtype FileExtension =
  FileExtension {
    name :: String
  }

fileExtension :: QuasiQuoter
fileExtension =
  QuasiQuoter {
    quoteExp = \case
      name
        | all isAsciiLower name -> [|FileExtension name|]
        | otherwise -> fail "Lower ASCII only"
  }
