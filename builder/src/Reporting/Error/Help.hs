{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Reporting.Error.Help
  ( Report
  , report
  , docReport
  , compilerReport
  , reportToDoc
  , hintLink
  , stack
  , reflow
  , note
  , toString
  , toStdout
  , toStderr
  )
  where

import qualified Data.List as List
import GHC.IO.Handle (hIsTerminalDevice)
import System.IO (Handle, hPutStr, stderr, stdout)
import qualified Text.PrettyPrint.ANSI.Leijen as P
import Text.PrettyPrint.ANSI.Leijen ((<>),(<+>))

import qualified Elm.Compiler as Compiler
import qualified Elm.Package as Pkg



-- REPORT


data Report
  = FromCompiler P.Doc
  | Report
      { _title :: String
      , _path :: Maybe FilePath
      , _start :: P.Doc
      , _others :: [P.Doc]
      }


report :: String -> Maybe FilePath -> String -> [P.Doc] -> Report
report title maybePath startString others =
  Report title maybePath (P.text startString) others


docReport :: String -> Maybe FilePath -> P.Doc -> [P.Doc] -> Report
docReport =
  Report


compilerReport :: P.Doc -> Report
compilerReport =
  FromCompiler


reportToDoc :: Report -> P.Doc
reportToDoc report_ =
  case report_ of
    FromCompiler doc ->
      doc

    Report title maybePath start others ->
      let
        makeDashes n =
          replicate (max 1 (80 - n)) '-'

        errorBarEnd =
          case maybePath of
            Nothing ->
              makeDashes (4 + length title)

            Just path ->
              makeDashes (5 + length title + length path) ++ " " ++ path

        errorBar =
          P.dullcyan $
            "--" <+> P.text title <+> P.text errorBarEnd
      in
        stack (errorBar : start : others ++ [""])



-- HINT LINKS


hintLink :: String -> String
hintLink hintName =
  "<https://github.com/elm-lang/elm-compiler/blob/"
  ++ Pkg.versionToString Compiler.version
  ++ "/hints/" ++ hintName ++ ".md>"



-- HELPERS


stack :: [P.Doc] -> P.Doc
stack allDocs =
  case allDocs of
    [] ->
      error "Do not use `stack` on empty lists."

    doc : docs ->
      List.foldl' verticalAppend doc docs


verticalAppend :: P.Doc -> P.Doc -> P.Doc
verticalAppend a b =
  a <> P.hardline <> P.hardline <> b


reflow :: String -> P.Doc
reflow paragraph =
  P.fillSep (map P.text (words paragraph))


note :: String -> P.Doc
note details =
  P.fillSep $
    (P.underline "Note" <> ":") : map P.text (words details)



-- OUTPUT


toString :: P.Doc -> String
toString doc =
  P.displayS (P.renderPretty 1 80 (P.plain doc)) ""


toStdout :: P.Doc -> IO ()
toStdout doc =
  toHandle stdout doc


toStderr :: P.Doc -> IO ()
toStderr doc =
  toHandle stderr doc


toHandle :: Handle -> P.Doc -> IO ()
toHandle handle doc =
  do  isTerminal <- hIsTerminalDevice handle
      if isTerminal
        then P.displayIO handle (P.renderPretty 1 80 doc)
        else hPutStr handle (toString doc)
