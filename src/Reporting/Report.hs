{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Reporting.Report
    ( Report
    , report
    , reportDoc
    , toDoc
    , toJson
    )
    where

import Data.Aeson ((.=))
import qualified Data.Aeson.Types as Json
import qualified Data.Text as Text
import Data.Text (Text)
import Text.PrettyPrint.ANSI.Leijen (Doc, (<>), displayS, dullcyan, hardline, plain, renderPretty, text)

import qualified Reporting.Helpers as Help
import qualified Reporting.Region as R
import qualified Reporting.Render.Code as Code



-- BUILD REPORTS


data Report =
  Report
    { _title :: Text
    , _highlight :: Maybe R.Region
    , _preHint :: Doc
    , _postHint :: Doc
    }


report :: Text -> Maybe R.Region -> Text -> Doc -> Report
report title highlight pre post =
  Report title highlight (Help.reflowParagraph pre) post


reportDoc :: Text -> Maybe R.Region -> [Doc] -> Doc -> Report
reportDoc title highlight pre post =
  Report title highlight (Help.hsep pre) post



-- REPORT TO JSON


toJson :: [Json.Pair] -> Report -> (Maybe R.Region, [Json.Pair])
toJson extraFields (Report title subregion pre post) =
  let
    fields =
      [ "tag" .= title
      , "overview" .= nonAnsiRender pre
      , "details" .= nonAnsiRender post
      ]
  in
    (subregion, fields ++ extraFields)


nonAnsiRender :: Doc -> String
nonAnsiRender doc =
  displayS (renderPretty 1 80 (plain doc)) ""



-- REPORT TO DOC


toDoc :: String -> R.Region -> Report -> Text -> Doc
toDoc location region (Report title highlight preHint postHint) source =
    messageBar title location
    <> hardline <> hardline <>
    preHint
    <> hardline <> hardline <>
    Code.render highlight region source
    <> hardline <>
    postHint
    <> hardline <> hardline


messageBar :: Text -> String -> Doc
messageBar tag location =
  let
    usedSpace =
      4 + Text.length tag + 1 + length location
  in
    dullcyan $ text $
      "-- " ++ Text.unpack tag
      ++ " " ++ replicate (max 1 (80 - usedSpace)) '-'
      ++ " " ++ location
