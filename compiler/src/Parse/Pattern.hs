{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}
module Parse.Pattern
  ( term
  , expression
  )
  where


import qualified Data.List as List
import qualified Data.Name as Name

import qualified AST.Source as Src
import Parse.Utils (Parser, SParser, SPos, checkSpace, spaces, whitespace)
import qualified Parse.Keyword as Keyword
import qualified Parse.Number as Number
import qualified Parse.Symbol as Symbol
import qualified Parse.Utf8 as Utf8
import qualified Parse.Variable as Var
import qualified Parse.Primitives as P
import Parse.Primitives (addLocation, getPosition, inContext, oneOf, word1)
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Syntax as E



-- TERM


term :: Parser Src.Pattern
term =
  do  start <- getPosition
      oneOf E.XXX
        [ record start
        , tuple start
        , list start
        , termHelp start
        ]


termHelp :: A.Position -> Parser Src.Pattern
termHelp start =
  oneOf E.XXX
    [
      do  Symbol.underscore
          end <- getPosition
          return (A.at start end Src.PAnything)
    ,
      do  name <- Var.lower
          end <- getPosition
          return (A.at start end (Src.PVar name))
    ,
      do  upper <- Var.foreignUpper
          end <- getPosition
          let region = A.Region start end
          return $ A.at start end $
            case upper of
              Var.Unqualified name ->
                Src.PCtor region name []

              Var.Qualified home name ->
                Src.PCtorQual region home name []
    ,
      do  number <- Number.number
          end <- getPosition
          case number of
            Number.Int int ->
              return (A.at start end (Src.PInt int))

            Number.Float _ ->
              noFloatsAllowedInPatterns
    ,
      do  str <- Utf8.string
          end <- getPosition
          return (A.at start end (Src.PStr str))
    ,
      do  chr <- Utf8.character
          end <- getPosition
          return (A.at start end (Src.PChr chr))
    ]


noFloatsAllowedInPatterns :: Parser a
noFloatsAllowedInPatterns =
  P.Parser $ \(P.State _ _ _ row col ctx) _ _ cerr _ ->
    cerr row col ctx E.FloatInPattern



-- RECORDS


record :: A.Position -> Parser Src.Pattern
record start =
  inContext E.ExprRecord (word1 0x7B {- { -} E.XXX) $
    do  spaces
        oneOf E.XXX
          [ do  var <- addLocation Var.lower
                spaces
                recordHelp start [var]
          , do  word1 0x7D {-}-} E.XXX
                end <- getPosition
                return (A.at start end (Src.PRecord []))
          ]


recordHelp :: A.Position -> [A.Located Name.Name] -> Parser Src.Pattern
recordHelp start vars =
  oneOf E.XXX
    [ do  word1 0x2C {-,-} E.XXX
          spaces
          var <- addLocation Var.lower
          spaces
          recordHelp start (var:vars)
    , do  word1 0x7D {-}-} E.XXX
          end <- getPosition
          return (A.at start end (Src.PRecord vars))
    ]



-- TUPLES


tuple :: A.Position -> Parser Src.Pattern
tuple start =
  inContext E.ExprTuple (word1 0x28 {-(-} E.XXX) $
    do  spaces
        oneOf E.XXX
          [ do  (pattern, sPos) <- expression
                checkSpace sPos
                tupleHelp start pattern []
          , do  word1 0x29 {-)-} E.XXX
                end <- getPosition
                return (A.at start end Src.PUnit)
          ]


tupleHelp :: A.Position -> Src.Pattern -> [Src.Pattern] -> Parser Src.Pattern
tupleHelp start firstPattern revPatterns =
  oneOf E.XXX
    [ do  word1 0x2C {-,-} E.XXX
          spaces
          (pattern, sPos) <- expression
          checkSpace sPos
          tupleHelp start firstPattern (pattern : revPatterns)
    , do  word1 0x29 {-)-} E.XXX
          case reverse revPatterns of
            [] ->
              return firstPattern

            secondPattern : otherPatterns ->
              do  end <- getPosition
                  return (A.at start end (Src.PTuple firstPattern secondPattern otherPatterns))
    ]



-- LIST


list :: A.Position -> Parser Src.Pattern
list start =
  inContext E.PatternList (word1 0x5B {-[-} E.XXX) $
    do  spaces
        oneOf E.XXX
          [ do  (pattern, sPos) <- expression
                checkSpace sPos
                listHelp start [pattern]
          , do  word1 0x5D {-]-} E.XXX
                end <- getPosition
                return (A.at start end (Src.PList []))
          ]


listHelp :: A.Position -> [Src.Pattern] -> Parser Src.Pattern
listHelp start patterns =
  oneOf E.XXX
    [ do  word1 0x2C {-,-} E.XXX
          spaces
          (pattern, sPos) <- expression
          checkSpace sPos
          listHelp start (pattern:patterns)
    , do  word1 0x5D {-]-} E.XXX
          end <- getPosition
          return (A.at start end (Src.PList (reverse patterns)))
    ]



-- EXPRESSION


expression :: Parser (Src.Pattern, SPos)
expression =
  do  start <- getPosition
      cTerm <- exprTerm
      exprHelp start [] cTerm


exprHelp :: A.Position -> [Src.Pattern] -> (Src.Pattern, A.Position, SPos) -> Parser (Src.Pattern, SPos)
exprHelp start patterns (pattern, _end, sPos) =
  oneOf E.XXX
    [ do  checkSpace sPos
          word1 0x3A {-:-} E.XXX
          spaces
          cTerm <- exprTerm
          exprHelp start (pattern:patterns) cTerm
    , do  checkSpace sPos
          Keyword.as_
          spaces
          nameStart <- getPosition
          name <- Var.lower
          newEnd <- getPosition
          newSpace <- whitespace
          let alias = A.at nameStart newEnd name
          return
            ( A.at start newEnd (Src.PAlias (List.foldl' cons pattern patterns) alias)
            , newSpace
            )
    , return
        ( List.foldl' cons pattern patterns
        , sPos
        )
    ]


cons :: Src.Pattern -> Src.Pattern -> Src.Pattern
cons tl hd =
  A.merge hd tl (Src.PCons hd tl)



-- EXPRESSION TERM


exprTerm :: SParser Src.Pattern
exprTerm =
  oneOf E.XXX
    [
      do  start <- getPosition
          upper <- Var.foreignUpper
          end <- getPosition
          exprTermHelp (A.Region start end) upper start []
    ,
      do  t@(A.At (A.Region _ end) _) <- term
          pos <- whitespace
          return (t, end, pos)
    ]


exprTermHelp :: A.Region -> Var.Upper -> A.Position -> [Src.Pattern] -> SParser Src.Pattern
exprTermHelp region upper start revArgs =
  do  end <- getPosition
      sPos <- whitespace
      oneOf E.XXX
        [ do  checkSpace sPos
              arg <- term
              exprTermHelp region upper start (arg:revArgs)
        , return
            ( A.at start end $
                case upper of
                  Var.Unqualified name ->
                    Src.PCtor region name (reverse revArgs)

                  Var.Qualified home name ->
                    Src.PCtorQual region home name (reverse revArgs)
            , end
            , sPos
            )
        ]
