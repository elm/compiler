{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Reporting.Report
    ( Report(..)
    , toDoc
    , toCodeSnippet
    , toCodePair
    )
    where

import qualified Data.Text as Text
import Text.PrettyPrint.ANSI.Leijen (Doc, (<>), hardline, dullcyan)

import qualified Reporting.Helpers as H
import qualified Reporting.Region as R
import qualified Reporting.Render.Code as Code



-- BUILD REPORTS


data Report =
  Report
    { _title :: String
    , _region :: R.Region
    , _sgstns :: [Text.Text]
    , _message :: Doc
    }


toDoc :: FilePath -> Report -> Doc
toDoc filePath (Report title _ _ message) =
  messageBar title filePath
  <> hardline <> hardline <>
  message


messageBar :: String -> FilePath -> Doc
messageBar title filePath =
  let
    usedSpace =
      4 + length title + 1 + length filePath
  in
    dullcyan $ H.text $
      "-- " ++ title
      ++ " " ++ replicate (max 1 (80 - usedSpace)) '-'
      ++ " " ++ filePath



-- CODE FORMATTING


toCodeSnippet :: Code.Source -> R.Region -> Maybe R.Region -> (Doc, Doc) -> Doc
toCodeSnippet source region highlight (preHint, postHint) =
  preHint
  <> hardline <> hardline <>
  Code.render source region highlight
  <> hardline <>
  postHint
  <> hardline <> hardline


toCodePair :: Code.Source -> R.Region -> R.Region -> (Doc, Doc) -> (Doc, Doc, Doc) -> Doc
toCodePair source r1 r2 (oneStart, oneEnd) (twoStart, twoMiddle, twoEnd) =
  case Code.renderPair source r1 r2 of
    Code.OneLine codeDocs ->
      oneStart
      <> hardline <> hardline <>
      codeDocs
      <> hardline <>
      oneEnd

    Code.TwoChunks code1 code2 ->
      twoStart
      <> hardline <> hardline <>
      code1
      <> hardline <>
      twoMiddle
      <> hardline <> hardline <>
      code2
      <> hardline <>
      twoEnd
