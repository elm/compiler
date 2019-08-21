{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Reporting.Render.Code
  ( Source
  , toSource
  , toSnippet
  , toPair
  , Next(..)
  , whatIsNext
  , nextLineStartsWithKeyword
  , nextLineStartsWithCloseCurly
  )
  where


import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as UTF8_BS
import qualified Data.Char as Char
import qualified Data.IntSet as IntSet
import qualified Data.List as List
import qualified Data.Name as Name
import qualified Data.Set as Set
import Data.Word (Word16)

import qualified Reporting.Annotation as A
import qualified Reporting.Doc as D
import Reporting.Doc (Doc)
import Parse.Primitives (Row, Col)
import Parse.Symbol (binopCharSet)
import Parse.Variable (reservedWords)



-- CODE


newtype Source =
  Source [(Word16, String)]


toSource :: B.ByteString -> Source
toSource source =
  Source $ zip [1..] $
    lines (UTF8_BS.toString source) ++ [""]



-- CODE FORMATTING


toSnippet :: Source -> A.Region -> Maybe A.Region -> (D.Doc, D.Doc) -> D.Doc
toSnippet source region highlight (preHint, postHint) =
  D.vcat
    [ preHint
    , ""
    , render source region highlight
    , postHint
    ]


toPair :: Source -> A.Region -> A.Region -> (D.Doc, D.Doc) -> (D.Doc, D.Doc, D.Doc) -> D.Doc
toPair source r1 r2 (oneStart, oneEnd) (twoStart, twoMiddle, twoEnd) =
  case renderPair source r1 r2 of
    OneLine codeDocs ->
      D.vcat
        [ oneStart
        , ""
        , codeDocs
        , oneEnd
        ]

    TwoChunks code1 code2 ->
      D.vcat
        [ twoStart
        , ""
        , code1
        , twoMiddle
        , ""
        , code2
        , twoEnd
        ]



-- RENDER SNIPPET


(|>) :: a -> (a -> b) -> b
(|>) a f =
  f a


render :: Source -> A.Region -> Maybe A.Region -> Doc
render (Source sourceLines) region@(A.Region (A.Position startLine _) (A.Position endLine _)) maybeSubRegion =
  let
    relevantLines =
      sourceLines
        |> drop (fromIntegral (startLine - 1))
        |> take (fromIntegral (1 + endLine - startLine))

    width =
      length (show (fst (last relevantLines)))

    smallerRegion =
      maybe region id maybeSubRegion
  in
    case makeUnderline width endLine smallerRegion of
      Nothing ->
        drawLines True width smallerRegion relevantLines D.empty

      Just underline ->
        drawLines False width smallerRegion relevantLines underline


makeUnderline :: Int -> Word16 -> A.Region -> Maybe Doc
makeUnderline width realEndLine (A.Region (A.Position start c1) (A.Position end c2)) =
  if start /= end || end < realEndLine then
    Nothing

  else
    let
      spaces = replicate (fromIntegral c1 + width + 1) ' '
      zigzag = replicate (max 1 (fromIntegral (c2 - c1))) '^'
    in
      Just (D.fromChars spaces <> D.red (D.fromChars zigzag))


drawLines :: Bool -> Int -> A.Region -> [(Word16, String)] -> Doc -> Doc
drawLines addZigZag width (A.Region (A.Position startLine _) (A.Position endLine _)) sourceLines finalLine =
  D.vcat $
    map (drawLine addZigZag width startLine endLine) sourceLines
    ++ [finalLine]


drawLine :: Bool -> Int -> Word16 -> Word16 -> (Word16, String) -> Doc
drawLine addZigZag width startLine endLine (n, line) =
  addLineNumber addZigZag width startLine endLine n (D.fromChars line)


addLineNumber :: Bool -> Int -> Word16 -> Word16 -> Word16 -> Doc -> Doc
addLineNumber addZigZag width start end n line =
  let
    number =
      show n

    lineNumber =
      replicate (width - length number) ' ' ++ number ++ "|"

    spacer =
      if addZigZag && start <= n && n <= end then
        D.red ">"
      else
        " "
  in
    D.fromChars lineNumber <> spacer <> line



-- RENDER PAIR


data CodePair
  = OneLine Doc
  | TwoChunks Doc Doc


renderPair :: Source -> A.Region -> A.Region -> CodePair
renderPair source@(Source sourceLines) region1 region2 =
  let
    (A.Region (A.Position startRow1 startCol1) (A.Position endRow1 endCol1)) = region1
    (A.Region (A.Position startRow2 startCol2) (A.Position endRow2 endCol2)) = region2
  in
  if startRow1 == endRow1 && endRow1 == startRow2 && startRow2 == endRow2 then
    let
      lineNumber = show startRow1
      spaces1 = replicate (fromIntegral startCol1 + length lineNumber + 1) ' '
      zigzag1 = replicate (fromIntegral (endCol1 - startCol1)) '^'
      spaces2 = replicate (fromIntegral (startCol2 - endCol1)) ' '
      zigzag2 = replicate (fromIntegral (endCol2 - startCol2)) '^'

      (Just line) = List.lookup startRow1 sourceLines
    in
    OneLine $
      D.vcat
        [ D.fromChars lineNumber <> "| " <> D.fromChars line
        , D.fromChars spaces1 <> D.red (D.fromChars zigzag1) <>
          D.fromChars spaces2 <> D.red (D.fromChars zigzag2)
        ]

  else
    TwoChunks
      (render source region1 Nothing)
      (render source region2 Nothing)



-- WHAT IS NEXT?


data Next
  = Keyword [Char]
  | Operator [Char]
  | Close [Char] Char
  | Upper Char [Char]
  | Lower Char [Char]
  | Other (Maybe Char)


whatIsNext :: Source -> Row -> Col -> Next
whatIsNext (Source sourceLines) row col =
  case List.lookup row sourceLines of
    Nothing ->
      Other Nothing

    Just line ->
      case drop (fromIntegral col - 1) line of
        [] ->
          Other Nothing

        c:cs
          | Char.isUpper c -> Upper c (takeWhile isInner cs)
          | Char.isLower c -> detectKeywords c cs
          | isSymbol c     -> Operator (c : takeWhile isSymbol cs)
          | c == ')'       -> Close "parenthesis" ')'
          | c == ']'       -> Close "square bracket" ']'
          | c == '}'       -> Close "curly brace" '}'
          | otherwise      -> Other (Just c)


detectKeywords :: Char -> [Char] -> Next
detectKeywords c rest =
  let
    cs = takeWhile isInner rest
    name = c : cs
  in
  if Set.member (Name.fromChars name) reservedWords
  then Keyword name
  else Lower c name


isInner :: Char -> Bool
isInner char =
  Char.isAlphaNum char || char == '_'


isSymbol :: Char -> Bool
isSymbol char =
  IntSet.member (Char.ord char) binopCharSet


startsWithKeyword :: [Char] -> [Char] -> Bool
startsWithKeyword restOfLine keyword =
  List.isPrefixOf keyword restOfLine
  &&
  case drop (length keyword) restOfLine of
    [] ->
      True

    c:_ ->
      not (isInner c)


nextLineStartsWithKeyword :: [Char] -> Source -> Row -> Maybe (Row, Col)
nextLineStartsWithKeyword keyword (Source sourceLines) row =
  case List.lookup (row + 1) sourceLines of
    Nothing ->
      Nothing

    Just line ->
      if startsWithKeyword (dropWhile (==' ') line) keyword then
        Just (row + 1, 1 + fromIntegral (length (takeWhile (==' ') line)))
      else
        Nothing


nextLineStartsWithCloseCurly :: Source -> Row -> Maybe (Row, Col)
nextLineStartsWithCloseCurly (Source sourceLines) row =
  case List.lookup (row + 1) sourceLines of
    Nothing ->
      Nothing

    Just line ->
      case dropWhile (==' ') line of
        '}':_ ->
          Just (row + 1, 1 + fromIntegral (length (takeWhile (==' ') line)))

        _ ->
          Nothing
