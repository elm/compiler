module Reporting.Report
    ( Report(Report)
    , simple
    , toString
    ) where

import Elm.Utils ((|>))
import qualified Reporting.Region as R


data Report = Report
    { highlight :: Maybe (Int,Int)
    , preHint :: String
    , postHint :: String
    }


simple :: String -> String -> Report
simple pre post =
  Report Nothing pre post


toString :: String -> String -> R.Region -> Report -> String -> String
toString tag location region report source =
  concat
    [ messageBar tag location
    , preHint report ++ "\n\n"
    , grabRegion (highlight report) region source ++ "\n"
    , postHint report ++ "\n\n\n"
    ]


-- REPORT HEADER

messageBar :: String -> String -> String
messageBar tag location =
  let usedSpace = 4 + length tag + 1 + length location
  in
      "-- " ++ tag ++ " "
      ++ replicate (max 1 (80 - usedSpace)) '-'
      ++ " " ++ location ++ "\n\n"


-- REGIONS

grabRegion :: Maybe (Int,Int) -> R.Region -> String -> String
grabRegion maybeSubRegion (R.Region start end) source =
  let
    (R.Position startLine startColumn) = start
    (R.Position endLine endColumn) = end

    relevantLines =
        lines source
          |> drop (startLine - 1)
          |> take (endLine - startLine + 1)
  in
  case relevantLines of
    [] ->
        error "something has gone badly wrong with reporting source locations"

    [theLine] ->
        let
          n = startLine
          w = length (show (n + 1))
          underline =
              replicate (startColumn - 1) ' '
              ++ replicate (max 1 (endColumn - startColumn)) '^'
          number =
              addLineNumber Nothing w
        in
            unlines
              [ number (n-1) ""
              , number n theLine
              , number (n+1) underline
              ]

    firstLine : rest ->
        let
          filteredFirstLine =
              replicate (startColumn - 1) ' '
              ++ drop (startColumn - 1) firstLine

          filteredLastLine =
              take (endColumn) (last rest)

          focusedRelevantLines =
              filteredFirstLine : init rest ++ [filteredLastLine]

          lineNumbersWidth =
              length (show (endLine + 1))

          numberedLines =
              zipWith
                (addLineNumber maybeSubRegion lineNumbersWidth)
                [startLine - 1 .. endLine + 1]
                ("" : focusedRelevantLines ++ [""])
        in
            unlines numberedLines ++ "\n"


addLineNumber :: Maybe (Int,Int) -> Int -> Int -> String -> String
addLineNumber maybeSubRegion width n line =
  let
    number = if n <= 0 then " " else show n
    spacer =
      case maybeSubRegion of
        Just (lo,hi) | lo <= n && n <= hi -> ">"
        _ -> " "
  in
    replicate (width - length number) ' ' ++ number ++ "|" ++ spacer ++ line
