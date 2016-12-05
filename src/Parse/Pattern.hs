{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}
module Parse.Pattern (term, expression) where

import Data.Text (Text)

import qualified AST.Literal as L
import qualified AST.Pattern as P
import qualified AST.Variable as Var
import Parse.Helpers
import qualified Parse.Literal as Literal
import qualified Reporting.Annotation as A
import qualified Reporting.Region as R



-- PATTERN TERMS


term :: Parser P.Raw
term =
  oneOf [ record, tuple, list, termHelp ]


termHelp :: Parser P.Raw
termHelp =
  addLocation $
    oneOf
      [ do  underscore
            return P.Anything
      , P.Var <$> lowVar
      , mkCtor <$> qualifiedCapVar
      , P.Literal <$> Literal.literal
      ]


mkCtor :: Text -> P.Raw'
mkCtor ctor =
  case ctor of
    "True" ->
      P.Literal (L.Boolean True)

    "False" ->
      P.Literal (L.Boolean False)

    _ ->
      P.Ctor (Var.Raw ctor) []



-- RECORDS


record :: Parser P.Raw
record =
  addLocation $
    do  leftCurly
        spaces
        oneOf
          [ do  var <- lowVar
                spaces
                recordHelp [var]
          , do  rightCurly
                return (P.Record [])
          ]


recordHelp :: [Text] -> Parser P.Raw'
recordHelp vars =
  oneOf
    [ do  comma
          spaces
          var <- lowVar
          spaces
          recordHelp (var:vars)
    , do  rightCurly
          return (P.Record vars)
    ]



-- TUPLES


tuple :: Parser P.Raw
tuple =
  do  start <- getPosition
      leftParen
      spaces
      oneOf
        [ do  (pattern, sPos) <- expression
              checkSpace sPos
              tupleHelp start [pattern]
        , do  rightParen
              end <- getPosition
              return (A.at start end (P.tuple []))
        ]


tupleHelp :: R.Position -> [P.Raw] -> Parser P.Raw
tupleHelp start patterns =
  oneOf
    [ do  comma
          spaces
          (pattern, sPos) <- expression
          checkSpace sPos
          tupleHelp start (pattern:patterns)
    , do  rightParen
          case patterns of
            [pattern] ->
              return pattern

            _ ->
              do  end <- getPosition
                  return (A.at start end (P.tuple (reverse patterns)))
    ]



-- LIST


list :: Parser P.Raw
list =
  do  leftSquare
      spaces
      oneOf
        [ do  (pattern, sPos) <- expression
              checkSpace sPos
              listHelp [pattern]
        , do  rightSquare
              end <- getPosition
              return (P.list end [])
        ]


listHelp :: [P.Raw] -> Parser P.Raw
listHelp patterns =
  oneOf
    [ do  comma
          spaces
          (pattern, sPos) <- expression
          checkSpace sPos
          listHelp (pattern:patterns)
    , do  end <- getPosition
          rightSquare
          return (P.list end (reverse patterns))
    ]



-- PATTERN EXPRESSION


expression :: Parser (P.Raw, SPos)
expression =
  do  start <- getPosition
      cTerm <- consTerm
      exprHelp start [] cTerm


consTerm :: SParser P.Raw
consTerm =
  oneOf
    [ do  start <- getPosition
          ctor <- qualifiedCapVar
          constructorHelp start ctor []
    , (,,) <$> term <*> getPosition <*> whitespace
    ]


exprHelp :: R.Position -> [P.Raw] -> (P.Raw, R.Position, SPos) -> Parser (P.Raw, SPos)
exprHelp start patterns (pattern, end, sPos) =
  oneOf
    [ do  checkSpace sPos
          cons
          spaces
          cTerm <- consTerm
          exprHelp start (pattern:patterns) cTerm
    , do  checkSpace sPos
          keyword "as"
          spaces
          alias <- lowVar
          newEnd <- getPosition
          newSpace <- whitespace
          return
            ( A.at start newEnd (P.Alias alias (foldl (consHelp end) pattern patterns))
            , newSpace
            )
    , return
        ( foldl (consHelp end) pattern patterns
        , sPos
        )
    ]


consHelp :: R.Position -> P.Raw -> P.Raw -> P.Raw
consHelp end tl hd@(A.A (R.Region start _) _) =
  A.at start end (P.Ctor (Var.Raw "::") [hd, tl])


constructorHelp :: R.Position -> Text -> [P.Raw] -> SParser P.Raw
constructorHelp start ctor args =
  do  end <- getPosition
      sPos <- whitespace
      oneOf
        [ do  checkSpace sPos
              arg <- term
              constructorHelp start ctor (arg:args)
        , return
            ( A.at start end (P.Ctor (Var.Raw ctor) (reverse args))
            , end
            , sPos
            )
        ]
