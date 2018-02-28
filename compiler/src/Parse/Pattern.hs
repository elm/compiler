{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}
module Parse.Pattern
  ( term
  , expression
  )
  where


import qualified Data.List as List

import qualified AST.Source as Src
import qualified Elm.Name as N
import Parse.Primitives (Parser, SParser, SPos, addLocation, checkSpace, getPosition, hint, inContext, spaces, oneOf)
import qualified Parse.Primitives as P
import qualified Parse.Primitives.Keyword as Keyword
import qualified Parse.Primitives.Number as Number
import qualified Parse.Primitives.Symbol as Symbol
import qualified Parse.Primitives.Utf8 as Utf8
import qualified Parse.Primitives.Variable as Var
import Parse.Primitives.Whitespace (whitespace)
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Syntax as E
import qualified Reporting.Region as R



-- TERM


term :: Parser Src.Pattern
term =
  hint E.Pattern $
    do  start <- getPosition
        oneOf
          [ record start
          , tuple start
          , list start
          , termHelp start
          ]


termHelp :: R.Position -> Parser Src.Pattern
termHelp start =
  oneOf
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
          let region = R.Region start end
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
              P.noFloatsAllowedInPatterns
    ,
      do  str <- Utf8.string
          end <- getPosition
          return (A.at start end (Src.PStr str))
    ,
      do  chr <- Utf8.character
          end <- getPosition
          return (A.at start end (Src.PChr chr))
    ]





-- RECORDS


record :: R.Position -> Parser Src.Pattern
record start =
  do  Symbol.leftCurly
      inContext start E.ExprRecord $
        do  spaces
            oneOf
              [ do  var <- addLocation Var.lower
                    spaces
                    recordHelp start [var]
              , do  Symbol.rightCurly
                    end <- getPosition
                    return (A.at start end (Src.PRecord []))
              ]


recordHelp :: R.Position -> [A.Located N.Name] -> Parser Src.Pattern
recordHelp start vars =
  oneOf
    [ do  Symbol.comma
          spaces
          var <- addLocation Var.lower
          spaces
          recordHelp start (var:vars)
    , do  Symbol.rightCurly
          end <- getPosition
          return (A.at start end (Src.PRecord vars))
    ]



-- TUPLES


tuple :: R.Position -> Parser Src.Pattern
tuple start =
  do  Symbol.leftParen
      inContext start E.ExprTuple $
        do  spaces
            oneOf
              [ do  (pattern, sPos) <- expression
                    checkSpace sPos
                    tupleHelp start pattern []
              , do  Symbol.rightParen
                    end <- getPosition
                    return (A.at start end Src.PUnit)
              ]


tupleHelp :: R.Position -> Src.Pattern -> [Src.Pattern] -> Parser Src.Pattern
tupleHelp start firstPattern revPatterns =
  oneOf
    [ do  Symbol.comma
          spaces
          (pattern, sPos) <- expression
          checkSpace sPos
          tupleHelp start firstPattern (pattern : revPatterns)
    , do  Symbol.rightParen
          case reverse revPatterns of
            [] ->
              return firstPattern

            secondPattern : otherPatterns ->
              do  end <- getPosition
                  return (A.at start end (Src.PTuple firstPattern secondPattern otherPatterns))
    ]



-- LIST


list :: R.Position -> Parser Src.Pattern
list start =
  do  Symbol.leftSquare
      inContext start E.PatternList $
        do  spaces
            oneOf
              [ do  (pattern, sPos) <- expression
                    checkSpace sPos
                    listHelp start [pattern]
              , do  Symbol.rightSquare
                    end <- getPosition
                    return (A.at start end (Src.PList []))
              ]


listHelp :: R.Position -> [Src.Pattern] -> Parser Src.Pattern
listHelp start patterns =
  oneOf
    [ do  Symbol.comma
          spaces
          (pattern, sPos) <- expression
          checkSpace sPos
          listHelp start (pattern:patterns)
    , do  Symbol.rightSquare
          end <- getPosition
          return (A.at start end (Src.PList (reverse patterns)))
    ]



-- EXPRESSION


expression :: Parser (Src.Pattern, SPos)
expression =
  hint E.Pattern $
    do  start <- getPosition
        cTerm <- exprTerm
        exprHelp start [] cTerm


exprHelp :: R.Position -> [Src.Pattern] -> (Src.Pattern, R.Position, SPos) -> Parser (Src.Pattern, SPos)
exprHelp start patterns (pattern, _end, sPos) =
  oneOf
    [ do  checkSpace sPos
          Symbol.cons
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
cons tl@(A.At (R.Region _ end) _) hd@(A.At (R.Region start _) _) =
  A.at start end (Src.PCons hd tl)



-- EXPRESSION TERM


exprTerm :: SParser Src.Pattern
exprTerm =
  oneOf
    [
      do  start <- getPosition
          upper <- Var.foreignUpper
          end <- getPosition
          exprTermHelp (R.Region start end) upper start []
    ,
      do  t@(A.At (R.Region _ end) _) <- term
          pos <- whitespace
          return (t, end, pos)
    ]


exprTermHelp :: R.Region -> Var.Upper -> R.Position -> [Src.Pattern] -> SParser Src.Pattern
exprTermHelp region upper start revArgs =
  do  end <- getPosition
      sPos <- whitespace
      oneOf
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
