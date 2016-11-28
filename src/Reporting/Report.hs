{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Reporting.Report
    ( Report
    , report
    , toString
    , toJson
    , toHandle
    )
    where

import Data.Aeson ((.=))
import qualified Data.Aeson.Types as Json
import System.IO (Handle)
import Text.PrettyPrint.ANSI.Leijen
    ( Doc, (<>), displayS, displayIO, dullcyan, fillSep
    , hardline, plain, renderPretty, text
    )

import qualified Reporting.Region as R
import qualified Reporting.Render.Code as Code



-- BUILD REPORTS


data Report = Report
    { _title :: String
    , _highlight :: Maybe R.Region
    , _preHint :: Doc
    , _postHint :: Doc
    }


report :: String -> Maybe R.Region -> String -> Doc -> Report
report title highlight pre post =
  Report title highlight (fillSep (map text (words pre))) post



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



-- REPORT TO DOC


toDoc :: Bool -> String -> R.Region -> Report -> String -> Doc
toDoc isEmacsStyle location region (Report title highlight preHint postHint) source =
  messageBar title location isEmacsStyle highlight region source
    <> hardline <> hardline <>
    preHint
    <> hardline <> hardline <>
    Code.render highlight region source
    <> hardline <>
    postHint
    <> hardline <> hardline


messageBar :: String -> String -> Bool -> Maybe R.Region -> R.Region -> String -> Doc
messageBar tag location isEmacsStyle highlight region source =
  let
    usedSpace =
      4 + length tag + 1 + length location
    normalHeader =
      "-- " ++ tag ++ " " ++ replicate (max 1 (80 - usedSpace)) '-' ++
      " " ++ location
    optionalLineAndColumnNumber =
      if isEmacsStyle then
        let
          (lineNumber, column) =
            Code.errorStartingPosition highlight region source
        in
          ":" ++ (show lineNumber) ++ ":" ++ (show column)
      else ""
  in
    dullcyan $ text $ normalHeader ++ optionalLineAndColumnNumber


-- RENDER DOCS


toHandle :: Handle -> Bool -> String -> R.Region -> Report -> String -> IO ()
toHandle handle isEmacsStyle location region rprt source =
  displayIO
    handle
    (renderPretty 1 80 (toDoc isEmacsStyle location region rprt source))


toString :: String -> Bool -> R.Region -> Report -> String -> String
toString location isEmacsStyle region rprt source =
  nonAnsiRender (toDoc isEmacsStyle location region rprt source)



-- DOC TO STRING WITH NO ANSI CODES


nonAnsiRender :: Doc -> String
nonAnsiRender doc =
  displayS (renderPretty 1 80 (plain doc)) ""

