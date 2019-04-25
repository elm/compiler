{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}
module Parse.Pattern
  ( term
  , expression
  )
  where


import qualified Data.List as List
import qualified Data.Name as Name
import qualified Data.Utf8 as Utf8

import qualified AST.Source as Src
import qualified Parse.Keyword as Keyword
import qualified Parse.Number as Number
import qualified Parse.Space as Space
import qualified Parse.Symbol as Symbol
import qualified Parse.Utf8 as Utf8
import qualified Parse.Variable as Var
import qualified Parse.Primitives as P
import Parse.Primitives (Parser, addLocation, addEnd, getPosition, inContext, oneOf, oneOfWithFallback, word1, word2)
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Syntax as E



-- TERM


term :: Parser E.Pattern Src.Pattern
term =
  do  start <- getPosition
      oneOf E.PStart
        [ record start
        , tuple start
        , list start
        , termHelp start
        ]


termHelp :: A.Position -> Parser E.Pattern Src.Pattern
termHelp start =
  oneOf E.PStart
    [
      do  Symbol.wildcard
          addEnd start Src.PAnything
    ,
      do  name <- Var.lower E.PStart
          addEnd start (Src.PVar name)
    ,
      do  upper <- Var.foreignUpper E.PStart
          end <- getPosition
          let region = A.Region start end
          return $ A.at start end $
            case upper of
              Var.Unqualified name ->
                Src.PCtor region name []

              Var.Qualified home name ->
                Src.PCtorQual region home name []
    ,
      do  number <- Number.number E.PStart E.PNumber
          end <- getPosition
          case number of
            Number.Int int ->
              return (A.at start end (Src.PInt int))

            Number.Float float ->
              P.Parser $ \(P.State _ _ _ row col) _ _ cerr _ ->
                cerr row col (E.PFloat (Utf8.size float))
    ,
      do  str <- Utf8.string E.PStart E.PString
          addEnd start (Src.PStr str)
    ,
      do  chr <- Utf8.character E.PStart E.PChar
          addEnd start (Src.PChr chr)
    ]



-- RECORDS


record :: A.Position -> Parser E.Pattern Src.Pattern
record start =
  inContext E.PRecord (word1 0x7B {- { -} E.PStart) $
    do  Space.chompAndCheckIndent E.PRecordSpace E.PRecordIndentOpen
        oneOf E.PRecordOpen
          [ do  var <- addLocation (Var.lower E.PRecordField)
                Space.chompAndCheckIndent E.PRecordSpace E.PRecordIndentEnd
                recordHelp start [var]
          , do  word1 0x7D {-}-} E.PRecordEnd
                addEnd start (Src.PRecord [])
          ]


recordHelp :: A.Position -> [A.Located Name.Name] -> Parser E.PRecord Src.Pattern
recordHelp start vars =
  oneOf E.PRecordEnd
    [ do  word1 0x2C {-,-} E.PRecordEnd
          Space.chompAndCheckIndent E.PRecordSpace E.PRecordIndentField
          var <- addLocation (Var.lower E.PRecordField)
          Space.chompAndCheckIndent E.PRecordSpace E.PRecordIndentEnd
          recordHelp start (var:vars)
    , do  word1 0x7D {-}-} E.PRecordEnd
          addEnd start (Src.PRecord vars)
    ]



-- TUPLES


tuple :: A.Position -> Parser E.Pattern Src.Pattern
tuple start =
  inContext E.PTuple (word1 0x28 {-(-} E.PStart) $
    do  Space.chompAndCheckIndent E.PTupleSpace E.PTupleIndentOpen
        oneOf E.PTupleOpen
          [ do  (pattern, end) <- P.specialize E.PTupleExpr expression
                Space.checkIndent end E.PTupleIndentEnd
                tupleHelp start pattern []
          , do  word1 0x29 {-)-} E.PTupleEnd
                addEnd start Src.PUnit
          ]


tupleHelp :: A.Position -> Src.Pattern -> [Src.Pattern] -> Parser E.PTuple Src.Pattern
tupleHelp start firstPattern revPatterns =
  oneOf E.PTupleEnd
    [ do  word1 0x2C {-,-} E.PTupleEnd
          Space.chompAndCheckIndent E.PTupleSpace E.PTupleIndentExpr
          (pattern, end) <- P.specialize E.PTupleExpr expression
          Space.checkIndent end E.PTupleIndentEnd
          tupleHelp start firstPattern (pattern : revPatterns)
    , do  word1 0x29 {-)-} E.PTupleEnd
          case reverse revPatterns of
            [] ->
              return firstPattern

            secondPattern : otherPatterns ->
              addEnd start (Src.PTuple firstPattern secondPattern otherPatterns)
    ]



-- LIST


list :: A.Position -> Parser E.Pattern Src.Pattern
list start =
  inContext E.PList (word1 0x5B {-[-} E.PStart) $
    do  Space.chompAndCheckIndent E.PListSpace E.PListIndentOpen
        oneOf E.PListOpen
          [ do  (pattern, end) <- P.specialize E.PListExpr expression
                Space.checkIndent end E.PListIndentEnd
                listHelp start [pattern]
          , do  word1 0x5D {-]-} E.PListEnd
                addEnd start (Src.PList [])
          ]


listHelp :: A.Position -> [Src.Pattern] -> Parser E.PList Src.Pattern
listHelp start patterns =
  oneOf E.PListEnd
    [ do  word1 0x2C {-,-} E.PListEnd
          Space.chompAndCheckIndent E.PListSpace E.PListIndentExpr
          (pattern, end) <- P.specialize E.PListExpr expression
          Space.checkIndent end E.PListIndentEnd
          listHelp start (pattern:patterns)
    , do  word1 0x5D {-]-} E.PListEnd
          addEnd start (Src.PList (reverse patterns))
    ]



-- EXPRESSION


expression :: Space.Parser E.Pattern Src.Pattern
expression =
  do  start <- getPosition
      cTerm <- exprTerm
      exprHelp start [] cTerm


exprHelp :: A.Position -> [Src.Pattern] -> (Src.Pattern, A.Position) -> Space.Parser E.Pattern Src.Pattern
exprHelp start patterns (pattern, end) =
  oneOfWithFallback
    [ do  Space.checkIndent end E.PIndentStart
          word2 0x3A 0x3A {-::-} E.PStart
          Space.chompAndCheckIndent E.PSpace E.PIndentStart
          cTerm <- exprTerm
          exprHelp start (pattern:patterns) cTerm
    , do  Space.checkIndent end E.PIndentStart
          Keyword.as_ E.PStart
          Space.chompAndCheckIndent E.PSpace E.PIndentAlias
          nameStart <- getPosition
          name <- Var.lower E.PAlias
          newEnd <- getPosition
          Space.chomp E.PSpace
          let alias = A.at nameStart newEnd name
          return
            ( A.at start newEnd (Src.PAlias (List.foldl' cons pattern patterns) alias)
            , newEnd
            )
    ]
    ( List.foldl' cons pattern patterns
    , end
    )


cons :: Src.Pattern -> Src.Pattern -> Src.Pattern
cons tl hd =
  A.merge hd tl (Src.PCons hd tl)



-- EXPRESSION TERM


exprTerm :: Space.Parser E.Pattern Src.Pattern
exprTerm =
  oneOf E.PStart
    [
      do  start <- getPosition
          upper <- Var.foreignUpper E.PStart
          end <- getPosition
          exprTermHelp (A.Region start end) upper start []
    ,
      do  eterm@(A.At (A.Region _ end) _) <- term
          Space.chomp E.PSpace
          return (eterm, end)
    ]


exprTermHelp :: A.Region -> Var.Upper -> A.Position -> [Src.Pattern] -> Space.Parser E.Pattern Src.Pattern
exprTermHelp region upper start revArgs =
  do  end <- getPosition
      Space.chomp E.PSpace
      oneOfWithFallback
        [ do  Space.checkIndent end E.PIndentStart
              arg <- term
              exprTermHelp region upper start (arg:revArgs)
        ]
        ( A.at start end $
            case upper of
              Var.Unqualified name ->
                Src.PCtor region name (reverse revArgs)

              Var.Qualified home name ->
                Src.PCtorQual region home name (reverse revArgs)
        , end
        )
