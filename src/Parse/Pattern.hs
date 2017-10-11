{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}
module Parse.Pattern
  ( term
  , expression
  )
  where


import qualified Data.List as List
import Data.Text (Text)

import qualified AST.Literal as L
import qualified AST.Pattern as P
import qualified Parse.Literal as Literal
import Parse.Primitives (Parser, SParser, SPos, checkSpace, getPosition, hint, inContext, spaces, oneOf)
import qualified Parse.Primitives.Keyword as Keyword
import qualified Parse.Primitives.Symbol as Symbol
import qualified Parse.Primitives.Variable as Var
import Parse.Primitives.Whitespace (whitespace)
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Syntax as E
import qualified Reporting.Region as R



-- TERM


term :: Parser P.Raw
term =
  hint E.Pattern $
    do  start <- getPosition
        oneOf
          [ record start
          , tuple start
          , list start
          , termHelp start
          ]


termHelp :: R.Position -> Parser P.Raw
termHelp start =
  do  pattern <-
        oneOf
          [ Symbol.underscore >> return P.RAnything
          , P.RVar <$> Var.lower
          , upperTerm <$> Var.foreignUpper
          , P.RLiteral <$> Literal.literal
          ]
      end <- getPosition
      return (A.at start end pattern)


upperTerm :: (Maybe Text, Text) -> P.Raw_
upperTerm pair =
  case pair of
    (Nothing, "True") ->
      P.RLiteral (L.Boolean True)

    (Nothing, "False") ->
      P.RLiteral (L.Boolean False)

    (maybePrefix, name) ->
      P.RCtor maybePrefix name []



-- RECORDS


record :: R.Position -> Parser P.Raw
record start =
  do  Symbol.leftCurly
      inContext start E.ExprRecord $
        do  spaces
            oneOf
              [ do  var <- Var.lower
                    spaces
                    recordHelp start [var]
              , do  Symbol.rightCurly
                    end <- getPosition
                    return (A.at start end (P.RRecord []))
              ]


recordHelp :: R.Position -> [Text] -> Parser P.Raw
recordHelp start vars =
  oneOf
    [ do  Symbol.comma
          spaces
          var <- Var.lower
          spaces
          recordHelp start (var:vars)
    , do  Symbol.rightCurly
          end <- getPosition
          return (A.at start end (P.RRecord vars))
    ]



-- TUPLES


tuple :: R.Position -> Parser P.Raw
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
                    return (A.at start end P.RUnit)
              ]


tupleHelp :: R.Position -> P.Raw -> [P.Raw] -> Parser P.Raw
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
                  return (A.at start end (P.RTuple firstPattern secondPattern otherPatterns))
    ]



-- LIST


list :: R.Position -> Parser P.Raw
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
                    return (A.at start end (P.RList []))
              ]


listHelp :: R.Position -> [P.Raw] -> Parser P.Raw
listHelp start patterns =
  oneOf
    [ do  Symbol.comma
          spaces
          (pattern, sPos) <- expression
          checkSpace sPos
          listHelp start (pattern:patterns)
    , do  Symbol.rightSquare
          end <- getPosition
          return (A.at start end (P.RList (reverse patterns)))
    ]



-- EXPRESSION


expression :: Parser (P.Raw, SPos)
expression =
  hint E.Pattern $
    do  start <- getPosition
        cTerm <- exprTerm
        exprHelp start [] cTerm


exprHelp :: R.Position -> [P.Raw] -> (P.Raw, R.Position, SPos) -> Parser (P.Raw, SPos)
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
          alias <- Var.lower
          newEnd <- getPosition
          newSpace <- whitespace
          return
            ( A.at start newEnd (P.RAlias alias (List.foldl' cons pattern patterns))
            , newSpace
            )
    , return
        ( List.foldl' cons pattern patterns
        , sPos
        )
    ]


cons :: P.Raw -> P.Raw -> P.Raw
cons tl@(A.A (R.Region _ end) _) hd@(A.A (R.Region start _) _) =
  A.at start end (P.RCons hd tl)



-- EXPRESSION TERM


exprTerm :: SParser P.Raw
exprTerm =
  oneOf
    [ do  start <- getPosition
          pair <- Var.foreignUpper
          exprTermHelp start pair []

    , (,,) <$> term <*> getPosition <*> whitespace
    ]


exprTermHelp :: R.Position -> (Maybe Text, Text) -> [P.Raw] -> SParser P.Raw
exprTermHelp start pair@(prefix, name) args =
  do  end <- getPosition
      sPos <- whitespace
      oneOf
        [ do  checkSpace sPos
              arg <- term
              exprTermHelp start pair (arg:args)
        , return
            ( A.at start end (P.RCtor prefix name (reverse args))
            , end
            , sPos
            )
        ]
