{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
{-# LANGUAGE MagicHash, OverloadedStrings #-}
module Reporting.Render.Code
  ( Source
  , toSource
  , toSnippet
  , toPair
  , Next(..)
  , whatIsNext
  , Outcome(..)
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
import GHC.Exts (Int(..))
import GHC.Prim
import GHC.Word (Word32)

import Parse.Primitives (Cursor, newline)
import Parse.Symbol (binopCharSet)
import Parse.Variable (reservedWords)
import qualified Reporting.Annotation as A
import qualified Reporting.Doc as D
import Reporting.Doc (Doc)



-- CODE


newtype Source =
  Source [(Word32, String)]


toSource :: B.ByteString -> Source
toSource source =
  Source $ zip [0..] $
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
render (Source sourceLines) region@(A.Region start end) maybeSubRegion =
  let
    startLine = fromIntegral $ A.toRow start
    endLine   = fromIntegral $ A.toRow end

    relevantLines =
      sourceLines
        |> drop (fromIntegral startLine)
        |> take (fromIntegral (1 + endLine - startLine))

    width =
      length (viewLineNumber (fst (last relevantLines)))

    smallerRegion =
      case maybeSubRegion of
        Just r  -> r
        Nothing -> region
  in
    case makeUnderline width endLine smallerRegion of
      Nothing ->
        drawLines True width smallerRegion relevantLines D.empty

      Just underline ->
        drawLines False width smallerRegion relevantLines underline


makeUnderline :: Int -> Word32 -> A.Region -> Maybe Doc
makeUnderline width realEndLine (A.Region p1 p2) =
  let
    startLine = fromIntegral $ A.toRow p1
    endLine   = fromIntegral $ A.toRow p2
  in
  if startLine /= endLine || endLine < realEndLine then
    Nothing

  else
    let
      c1     = A.toCol p1
      c2     = A.toCol p2
      spaces = replicate (fromIntegral c1 + width + 1) ' '
      zigzag = replicate (max 1 (fromIntegral (c2 - c1))) '^'
    in
      Just (D.fromChars spaces <> D.red (D.fromChars zigzag))


drawLines :: Bool -> Int -> A.Region -> [(Word32, String)] -> Doc -> Doc
drawLines addZigZag width (A.Region start end) sourceLines finalLine =
  let
    startLine = fromIntegral $ A.toRow start
    endLine   = fromIntegral $ A.toRow end
  in
  D.vcat $
    map (drawLine addZigZag width startLine endLine) sourceLines
    ++ [finalLine]


drawLine :: Bool -> Int -> Word32 -> Word32 -> (Word32, String) -> Doc
drawLine addZigZag width startLine endLine (n, line) =
  addLineNumber addZigZag width startLine endLine n (D.fromChars line)


addLineNumber :: Bool -> Int -> Word32 -> Word32 -> Word32 -> Doc -> Doc
addLineNumber addZigZag width start end n line =
  let
    number =
      viewLineNumber n

    lineNumber =
      replicate (width - length number) ' ' ++ number ++ "|"

    spacer =
      if addZigZag && start <= n && n <= end then
        D.red ">"
      else
        " "
  in
    D.fromChars lineNumber <> spacer <> line


viewLineNumber :: Word32 -> String
viewLineNumber row =
  show (row + 1)



-- RENDER PAIR


data CodePair
  = OneLine Doc
  | TwoChunks Doc Doc


renderPair :: Source -> A.Region -> A.Region -> CodePair
renderPair source@(Source sourceLines) r1@(A.Region s1 e1) r2@(A.Region s2 e2) =
  let
    startRow1 = A.toRow s1
    startCol1 = A.toCol s1
    endRow1   = A.toRow e1
    endCol1   = A.toCol e1
    startRow2 = A.toRow s2
    startCol2 = A.toCol s2
    endRow2   = A.toRow e2
    endCol2   = A.toCol e2
  in
  if startRow1 == endRow1 && endRow1 == startRow2 && startRow2 == endRow2 then
    let
      lineNumber = show startRow1
      spaces1 = replicate (fromIntegral startCol1 + length lineNumber + 2) ' '
      zigzag1 = replicate (fromIntegral (endCol1 - startCol1)) '^'
      spaces2 = replicate (fromIntegral (startCol2 - endCol1)) ' '
      zigzag2 = replicate (fromIntegral (endCol2 - startCol2)) '^'

      (_,line) = List.genericIndex sourceLines startRow1
    in
    OneLine $
      D.vcat
        [ D.fromChars lineNumber <> "| " <> D.fromChars line
        , D.fromChars spaces1 <> D.red (D.fromChars zigzag1) <>
          D.fromChars spaces2 <> D.red (D.fromChars zigzag2)
        ]

  else
    TwoChunks
      (render source r1 Nothing)
      (render source r2 Nothing)



-- WHAT IS NEXT?


data Next
  = Keyword [Char]
  | Operator [Char]
  | Close [Char] Char
  | Upper Char [Char]
  | Lower Char [Char]
  | Other (Maybe Char)


whatIsNext :: Source -> Cursor -> Next
whatIsNext (Source sourceLines) cur =
  case List.lookup row sourceLines of
    Nothing ->
      Other Nothing

    Just line ->
      case drop (col - 1) line of
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
  where
    row = fromIntegral $ A.toRow cur
    col = fromIntegral $ A.toCol cur


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


data Outcome
  = Yes Cursor
  | No


nextLineStartsWithKeyword :: [Char] -> Source -> Cursor -> Outcome
nextLineStartsWithKeyword keyword (Source sourceLines) cur =
  case List.lookup next sourceLines of
    Nothing ->
      No

    Just line ->
      if startsWithKeyword (List.dropWhile (==' ') line) keyword
      then Yes (newline cur `plusWord64#` countSpaces line)
      else No
  where
    next = fromIntegral (A.toRow cur) + 1


nextLineStartsWithCloseCurly :: Source -> Cursor -> Outcome
nextLineStartsWithCloseCurly (Source sourceLines) cur =
  case List.lookup next sourceLines of
    Nothing ->
      No

    Just line ->
      case List.dropWhile (==' ') line of
        '}':_ -> Yes (newline cur `plusWord64#` countSpaces line)
        _     -> No
  where
    next = fromIntegral (A.toRow cur) + 1


countSpaces :: String -> Word64#
countSpaces line =
  case List.length (List.takeWhile (==' ') line) of
    I# len ->
      wordToWord64# (int2Word# len)
