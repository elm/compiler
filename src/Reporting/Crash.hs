{-# OPTIONS_GHC -Wall #-}
module Reporting.Crash where

import qualified Reporting.Region as R


data Details
    = IncompleteMultiIf R.Region
    | IncompletePatternMatch
    deriving (Show)


summary :: Details -> String
summary details =
  case details of
    IncompleteMultiIf _ ->
        "incomplete-multi-if"

    IncompletePatternMatch ->
        "incomplete-pattern-match"