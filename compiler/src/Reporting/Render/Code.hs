{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Reporting.Render.Code
  ( Source
  , toSource
  , render
  , CodePair(..)
  , renderPair
  )
  where


import qualified Data.List as List
import qualified Data.Text as Text
import Text.PrettyPrint.ANSI.Leijen (Doc, (<>), hardline, dullred, empty, text)

import qualified Reporting.Region as R



-- CODE


newtype Source =
  Source [(Int, Text.Text)]


toSource :: Text.Text -> Source
toSource source =
  Source $ zip [1..] $
    Text.lines source ++ [Text.empty]



-- RENDER


(|>) :: a -> (a -> b) -> b
(|>) a f =
  f a


(<==>) :: Doc -> Doc -> Doc
(<==>) a b =
  a <> hardline <> b


render :: Source -> R.Region -> Maybe R.Region -> Doc
render (Source sourceLines) region@(R.Region start end) maybeSubRegion =
  let
    (R.Position startLine _) = start
    (R.Position endLine _) = end

    relevantLines =
      sourceLines
        |> drop (startLine - 1)
        |> take (1 + endLine - startLine)

    width =
      length (show (fst (last relevantLines)))

    smallerRegion =
      maybe region id maybeSubRegion
  in
    case makeUnderline width endLine smallerRegion of
      Nothing ->
        drawLines True width smallerRegion relevantLines empty

      Just underline ->
        drawLines False width smallerRegion relevantLines underline


makeUnderline :: Int -> Int -> R.Region -> Maybe Doc
makeUnderline width realEndLine (R.Region (R.Position start c1) (R.Position end c2)) =
  if start /= end || end < realEndLine then
    Nothing

  else
    let
      spaces = replicate (c1 + width + 1) ' '
      zigzag = replicate (max 1 (c2 - c1)) '^'
    in
      Just (text spaces <> dullred (text zigzag))


drawLines :: Bool -> Int -> R.Region -> [(Int, Text.Text)] -> Doc -> Doc
drawLines addZigZag width (R.Region start end) sourceLines finalLine =
  let
    (R.Position startLine _) = start
    (R.Position endLine _) = end
  in
    foldr (<==>) finalLine $
      map (drawLine addZigZag width startLine endLine) sourceLines


drawLine :: Bool -> Int -> Int -> Int -> (Int, Text.Text) -> Doc
drawLine addZigZag width startLine endLine (n, line) =
  addLineNumber addZigZag width startLine endLine n (text (Text.unpack line))


addLineNumber :: Bool -> Int -> Int -> Int -> Int -> Doc -> Doc
addLineNumber addZigZag width start end n line =
  let
    number =
      if n < 0 then " " else show n

    lineNumber =
      replicate (width - length number) ' ' ++ number ++ "|"

    spacer =
      if addZigZag && start <= n && n <= end then
        dullred ">"
      else
        " "
  in
    text lineNumber <> spacer <> line



-- RENDER PAIR


data CodePair
  = OneLine Doc
  | TwoChunks Doc Doc


renderPair :: Source -> R.Region -> R.Region -> CodePair
renderPair source@(Source sourceLines) region1 region2 =
  let
    (R.Region (R.Position startRow1 startCol1) (R.Position endRow1 endCol1)) = region1
    (R.Region (R.Position startRow2 startCol2) (R.Position endRow2 endCol2)) = region2
  in
  if startRow1 == endRow1 && endRow1 == startRow2 && startRow2 == endRow2 then
    let
      lineNumber = show startRow1
      spaces1 = replicate (startCol1 + length lineNumber + 1) ' '
      zigzag1 = replicate (endCol1 - startCol1) '^'
      spaces2 = replicate (startCol2 - endCol1) ' '
      zigzag2 = replicate (endCol2 - startCol2) '^'

      (Just line) = List.lookup startRow1 sourceLines
    in
    OneLine $
      text lineNumber <> "| " <> text (Text.unpack line)
      <> hardline
      <> text spaces1 <> dullred (text zigzag1)
      <> text spaces2 <> dullred (text zigzag2)

  else
    TwoChunks
      (render source region1 Nothing)
      (render source region2 Nothing)
