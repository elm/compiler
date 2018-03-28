{-# LANGUAGE OverloadedStrings #-}
module Reporting.Exit.Help
  ( Report
  , report
  , docReport
  , jsonReport
  , compilerReport
  , reportToDoc
  , reportToJson
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
import qualified Data.Text as Text
import GHC.IO.Handle (hIsTerminalDevice)
import System.IO (Handle, hPutStr, stderr, stdout)
import qualified Text.PrettyPrint.ANSI.Leijen as P
import Text.PrettyPrint.ANSI.Leijen ((<>),(<+>))

import qualified Elm.Compiler as Compiler
import qualified Elm.Package as Pkg
import qualified Json.Encode as Encode
import qualified Reporting.Exit.Compile as Compile



-- REPORT


data Report
  = CompilerReport Compile.Exit [Compile.Exit]
  | Report
      { _title :: String
      , _path :: Maybe FilePath
      , _message :: P.Doc
      }


report :: String -> Maybe FilePath -> String -> [P.Doc] -> Report
report title path startString others =
  Report title path $ stack (reflow startString:others)


docReport :: String -> Maybe FilePath -> P.Doc -> [P.Doc] -> Report
docReport title path startDoc others =
  Report title path $ stack (startDoc:others)


jsonReport :: String -> Maybe FilePath -> P.Doc -> Report
jsonReport =
  Report


compilerReport :: Compile.Exit -> [Compile.Exit] -> Report
compilerReport =
  CompilerReport



-- TO DOC


reportToDoc :: Report -> P.Doc
reportToDoc report_ =
  case report_ of
    CompilerReport e es ->
      Compile.toDoc e es

    Report title maybePath message ->
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
        stack [errorBar, message, ""]



-- TO JSON


reportToJson :: Report -> Encode.Value
reportToJson report_ =
  case report_ of
    CompilerReport e es ->
      Encode.object
        [ ("type", Encode.text "compile-errors")
        , ("errors", Encode.list Compile.toJson (e:es))
        ]

    Report title maybePath message ->
      let
        messageString =
          P.displayS (P.renderPretty 1 80 message) ""
      in
      Encode.object
        [ ("type", Encode.text "error")
        , ("path", maybe Encode.null (Encode.text . Text.pack) maybePath)
        , ("title", Encode.text (Text.pack title))
        , ("message", Encode.text (Text.pack messageString))
        ]



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
