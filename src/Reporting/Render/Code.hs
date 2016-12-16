{-# OPTIONS_GHC -Wall #-}
module Reporting.Render.Code (render) where

import Data.Text (Text)
import qualified Data.Text as Text
import Text.PrettyPrint.ANSI.Leijen
  ( Doc, (<>), hardline, dullred, empty, text )

import qualified Reporting.Region as R
import qualified Reporting.Helpers as H


(|>) :: a -> (a -> b) -> b
(|>) a f =
  f a


(<==>) :: Doc -> Doc -> Doc
(<==>) a b =
  a <> hardline <> b


render :: Maybe R.Region -> R.Region -> Text -> Doc
render maybeSubRegion region@(R.Region start end) source =
  let
    (R.Position startLine _) = start
    (R.Position endLine _) = end

    relevantLines =
      (Text.lines source ++ [Text.empty])
        |> take endLine
        |> drop (startLine - 1)
        |> zip [startLine .. endLine + 1]

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


drawLines :: Bool -> Int -> R.Region -> [(Int, Text)] -> Doc -> Doc
drawLines addZigZag width (R.Region start end) sourceLines finalLine =
  let
    (R.Position startLine _) = start
    (R.Position endLine _) = end
  in
    foldr (<==>) finalLine $
      map (drawLine addZigZag width startLine endLine) sourceLines


drawLine :: Bool -> Int -> Int -> Int -> (Int, Text) -> Doc
drawLine addZigZag width startLine endLine (n, line) =
  addLineNumber addZigZag width startLine endLine n (H.text line)


addLineNumber :: Bool -> Int -> Int -> Int -> Int -> Doc -> Doc
addLineNumber addZigZag width start end n line =
  let
    number =
      if n < 0 then " " else show n

    lineNumber =
      replicate (width - length number) ' ' ++ number ++ "|"

    spacer =
      if addZigZag && start <= n && n <= end then
        dullred (text ">")
      else
        text " "
  in
    text lineNumber <> spacer <> line
