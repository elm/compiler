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
  (if isEmacsStyle
   then
     let
       (lineNumber, column) =
         Code.errorStartingPosition highlight region source
     in
       emacsMessageBar title location lineNumber column
   else
     messageBar title location)
    <> hardline <> hardline <>
    preHint
    <> hardline <> hardline <>
    Code.render highlight region source
    <> hardline <>
    postHint
    <> hardline <> hardline


messageBar :: String -> String -> Doc
messageBar tag location =
  let
    usedSpace =
      4 + length tag + 1 + length location
  in
    dullcyan $ text $
      "-- " ++ tag ++ " " ++ replicate (max 1 (80 - usedSpace)) '-' ++ " " ++ location


emacsMessageBar :: String -> String -> Int -> Int -> Doc
emacsMessageBar tag location lineNumber column =
  let
    lineLoc =
      location ++ ":" ++ (show lineNumber) ++ ":" ++ (show column) ++ ":"
    usedSpace =
      4 + length tag + 1 + length location
    mainHeader =
      "-- " ++ tag ++ " " ++ replicate (max 1 (80 - usedSpace)) '-'
  in
    dullcyan $ text $ mainHeader ++ "\n" ++ lineLoc


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

