{-# LANGUAGE BangPatterns, ExtendedLiterals, MagicHash, UnboxedTuples #-}
module Parse.Pattern
  ( term
  , expression
  )
  where


import qualified Data.List as List
import qualified Data.Name as Name
import qualified Data.Utf8 as Utf8
import GHC.Prim

import qualified AST.Source as Src
import qualified AST.Prim.Variable as Var
import qualified Parse.Keyword as Keyword
import qualified Parse.Number as Number
import qualified Parse.Space as Space
import qualified Parse.String as String
import qualified Parse.Variable as Var
import qualified Parse.Primitives as P
import Parse.Primitives (Parser, slide, neIndex, eqAddr, ltAddr, addLocation, addEnd, getPosition, inContext, oneOf, oneOfWithFallback, word1, word2)
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Syntax as E



-- TERM


term :: Parser E.Pattern Src.Pattern
term =
  termHelp =<< getPosition


termHelp :: A.Position -> Parser E.Pattern Src.Pattern
termHelp start =
  oneOf E.PStart
    [
      record start
    ,
      tuple start
    ,
      list start
    ,
      do  wildcard
          addEnd start Src.PAnything
    ,
      do  name <- Var.lower E.PStart
          addEnd start (Src.PVar name)
    ,
      do  upper <- Var.foreignUpper E.PStart
          end <- getPosition
          let region = A.region start end
          return $ A.At region $
            case upper of
              Var.Unqualified n -> Src.PCtor     region   n []
              Var.Qualified h n -> Src.PCtorQual region h n []
    ,
      do  number <- Number.number E.PStart E.PNumber
          end <- getPosition
          case number of
            Number.Int int ->
              return (A.at start end (Src.PInt int))

            Number.Float float ->
              P.Parser $ \_ (P.State _ _ _ cur) _ _ cerr _ ->
                let
                  !width = wordToWord64# (int2Word# (Utf8.size# float))
                in
                cerr (subWord64# cur width) (E.PFloat width)
    ,
      do  str <- String.string E.PStart E.PString
          addEnd start (Src.PStr str)
    ,
      do  chr <- String.character E.PStart E.PChar
          addEnd start (Src.PChr chr)
    ]



-- WILDCARD


wildcard :: Parser E.Pattern ()
wildcard =
  P.Parser $ \_ (P.State pos end indent cur) cok _ cerr eerr ->
    if eqAddr pos end || neIndex pos 0# 0x5F#Word8 {- _ -}
    then eerr cur E.PStart
    else
      let
        !newPos = plusAddr# pos 1#
        !(# badPos, badOffset #) = chompInners newPos end
      in
      case badOffset of
        0#Word64 ->
          do  let !newState = P.State newPos end indent (slide cur 1#Word64)
              cok () newState

        _ ->
          do  name <- Utf8.fromAddr pos badPos
              cerr cur (E.PWildcardNotVar name badOffset)


chompInners :: Addr# -> Addr# -> (# Addr#, Word64# #)
chompInners start end =
    loop start 0#Word64
  where
    loop pos off =
      if ltAddr pos end
      then
        let
          !newPos = Var.chompInner pos end (indexWord8OffAddr# pos 0#)
        in
        if ltAddr pos newPos
        then loop newPos (plusWord64# off 1#Word64)
        else (# pos, off #)
      else
        (# pos, off #)



-- RECORDS


record :: A.Position -> Parser E.Pattern Src.Pattern
record start =
  inContext E.PRecord (word1 0x7B#Word8 {- { -} E.PStart) $
    do  Space.chompAndCheckIndent E.PRecordSpace E.PRecordIndentOpen
        oneOf E.PRecordOpen
          [ do  name <- addLocation (Var.lower E.PRecordField)
                Space.chompAndCheckIndent E.PRecordSpace E.PRecordIndentEnd
                recordHelp start [name]
          , do  word1 0x7D#Word8 {-}-} E.PRecordEnd
                addEnd start (Src.PRecord [])
          ]


recordHelp :: A.Position -> [A.Located Name.Name] -> Parser E.PRecord Src.Pattern
recordHelp start names =
  oneOf E.PRecordEnd
    [ do  word1 0x2C#Word8 {-,-} E.PRecordEnd
          Space.chompAndCheckIndent E.PRecordSpace E.PRecordIndentField
          name <- addLocation (Var.lower E.PRecordField)
          Space.chompAndCheckIndent E.PRecordSpace E.PRecordIndentEnd
          recordHelp start (name:names)
    , do  word1 0x7D#Word8 {-}-} E.PRecordEnd
          addEnd start (Src.PRecord (List.reverse names))
    ]



-- TUPLES


tuple :: A.Position -> Parser E.Pattern Src.Pattern
tuple start =
  inContext E.PTuple (word1 0x28#Word8 {-(-} E.PStart) $
    do  Space.chompAndCheckIndent E.PTupleSpace E.PTupleIndentExpr1
        oneOf E.PTupleOpen
          [ do  (pattern, end) <- P.specialize E.PTupleExpr expression
                Space.checkIndent end E.PTupleIndentEnd
                tupleHelp start pattern []
          , do  word1 0x29#Word8 {-)-} E.PTupleEnd
                addEnd start Src.PUnit
          ]


tupleHelp :: A.Position -> Src.Pattern -> [Src.Pattern] -> Parser E.PTuple Src.Pattern
tupleHelp start firstPattern revPatterns =
  oneOf E.PTupleEnd
    [ do  word1 0x2C#Word8 {-,-} E.PTupleEnd
          Space.chompAndCheckIndent E.PTupleSpace E.PTupleIndentExprN
          (pattern, end) <- P.specialize E.PTupleExpr expression
          Space.checkIndent end E.PTupleIndentEnd
          tupleHelp start firstPattern (pattern : revPatterns)
    , do  word1 0x29#Word8 {-)-} E.PTupleEnd
          case List.reverse revPatterns of
            [] ->
              return firstPattern

            secondPattern : otherPatterns ->
              addEnd start (Src.PTuple firstPattern secondPattern otherPatterns)
    ]



-- LIST


list :: A.Position -> Parser E.Pattern Src.Pattern
list start =
  inContext E.PList (word1 0x5B#Word8 {-[-} E.PStart) $
    do  Space.chompAndCheckIndent E.PListSpace E.PListIndentOpen
        oneOf E.PListOpen
          [ do  (pattern, end) <- P.specialize E.PListExpr expression
                Space.checkIndent end E.PListIndentEnd
                listHelp start [pattern]
          , do  word1 0x5D#Word8 {-]-} E.PListEnd
                addEnd start (Src.PList [])
          ]


listHelp :: A.Position -> [Src.Pattern] -> Parser E.PList Src.Pattern
listHelp start patterns =
  oneOf E.PListEnd
    [ do  word1 0x2C#Word8 {-,-} E.PListEnd
          Space.chompAndCheckIndent E.PListSpace E.PListIndentExpr
          (pattern, end) <- P.specialize E.PListExpr expression
          Space.checkIndent end E.PListIndentEnd
          listHelp start (pattern:patterns)
    , do  word1 0x5D#Word8 {-]-} E.PListEnd
          addEnd start (Src.PList (List.reverse patterns))
    ]



-- EXPRESSION


expression :: Space.Parser E.Pattern Src.Pattern
expression =
  do  start <- getPosition
      ePart <- exprPart
      exprHelp start [] ePart


exprHelp :: A.Position -> [Src.Pattern] -> (Src.Pattern, A.Position) -> Space.Parser E.Pattern Src.Pattern
exprHelp start revPatterns (pattern, end) =
  oneOfWithFallback
    [ do  Space.checkIndent end E.PIndentStart
          word2 0x3A#Word8 0x3A#Word8 {-::-} E.PStart
          Space.chompAndCheckIndent E.PSpace E.PIndentStart
          ePart <- exprPart
          exprHelp start (pattern:revPatterns) ePart
    , do  Space.checkIndent end E.PIndentStart
          Keyword.as_ E.PStart
          Space.chompAndCheckIndent E.PSpace E.PIndentAlias
          nameStart <- getPosition
          name <- Var.lower E.PAlias
          newEnd <- getPosition
          Space.chomp E.PSpace
          let alias = A.at nameStart newEnd name
          return
            ( A.at start newEnd (Src.PAlias (List.foldl' cons pattern revPatterns) alias)
            , newEnd
            )
    ]
    ( List.foldl' cons pattern revPatterns
    , end
    )


cons :: Src.Pattern -> Src.Pattern -> Src.Pattern
cons tl hd =
  A.merge hd tl (Src.PCons hd tl)



-- EXPRESSION PART


exprPart :: Space.Parser E.Pattern Src.Pattern
exprPart =
  oneOf E.PStart
    [
      do  start <- getPosition
          upper <- Var.foreignUpper E.PStart
          end <- getPosition
          exprPartArgs (A.region start end) upper start []
    ,
      do  eterm@(A.At (A.Region _ end) _) <- term
          Space.chomp E.PSpace
          return (eterm, A.Position end)
    ]


exprPartArgs :: A.Region -> Var.Upper -> A.Position -> [Src.Pattern] -> Space.Parser E.Pattern Src.Pattern
exprPartArgs region upper start revArgs =
  do  end <- getPosition
      Space.chomp E.PSpace
      oneOfWithFallback
        [ do  Space.checkIndent end E.PIndentStart
              arg <- term
              exprPartArgs region upper start (arg:revArgs)
        ]
        ( A.at start end $
            case upper of
              Var.Unqualified n -> Src.PCtor     region   n (List.reverse revArgs)
              Var.Qualified h n -> Src.PCtorQual region h n (List.reverse revArgs)
        , end
        )

