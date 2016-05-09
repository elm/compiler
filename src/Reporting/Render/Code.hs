{-# OPTIONS_GHC -Wall #-}
module Reporting.Render.Code
    ( render
    )
    where

import Control.Applicative ((<|>))
import Text.PrettyPrint.ANSI.Leijen (Doc, (<>), hardline, dullred, empty, text)

import qualified Reporting.Region as R


(|>) :: a -> (a -> b) -> b
(|>) a f =
  f a


(<==>) :: Doc -> Doc -> Doc
(<==>) a b =
  a <> hardline <> b


render :: Maybe R.Region -> R.Region -> String -> Doc
render maybeSubRegion region@(R.Region start end) source =
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
          empty

      [sourceLine] ->
          singleLineRegion startLine sourceLine $
            case maybeSubRegion of
              Nothing ->
                  (0, startColumn, endColumn, length sourceLine)

              Just (R.Region s e) ->
                  (startColumn, R.column s, R.column e, endColumn)

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
                length (show endLine)

            subregion =
                maybeSubRegion <|> Just region

            numberedLines =
                zipWith
                  (addLineNumber subregion lineNumbersWidth)
                  [startLine .. endLine]
                  focusedRelevantLines
          in
            foldr (<==>) empty numberedLines


addLineNumber :: Maybe R.Region -> Int -> Int -> String -> Doc
addLineNumber maybeSubRegion width n line =
  let
    number =
      if n < 0 then " " else show n

    lineNumber =
      replicate (width - length number) ' ' ++ number ++ "|"

    spacer (R.Region start end) =
      if R.line start <= n && n <= R.line end then
          dullred (text ">")
      else
          text " "
  in
    text lineNumber
    <> maybe (text " ") spacer maybeSubRegion
    <> text line


singleLineRegion :: Int -> String -> (Int, Int, Int, Int) -> Doc
singleLineRegion lineNum sourceLine (start, innerStart, innerEnd, end) =
  let
    width =
      length (show lineNum)

    underline =
      text (replicate (innerStart + width + 1) ' ')
      <>
      dullred (text (replicate (max 1 (innerEnd - innerStart)) '^'))

    trimmedSourceLine =
        sourceLine
          |> drop (start - 1)
          |> take (end - start + 1)
          |> (++) (replicate (start - 1) ' ')
  in
    addLineNumber Nothing width lineNum trimmedSourceLine
    <==>
    underline
