{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}
module Parse.Primitives
  ( I.Parser
  , I.oneOf
  , run, runAt
  , try, deadend, hint, endOfFile
  , noFloatsAllowedInPatterns
  , getPosition, getCol
  , pushContext, popContext
  , getIndent, setIndent
  , W.SPos
  , SParser
  , addLocation, inContext
  , spaces, noSpace, checkSpace, checkAligned, checkFreshLine
  )
  where


import Prelude hiding (length)
import qualified Data.ByteString.Internal as B

import Parse.Primitives.Internals (Parser(..), State(..), expect, noError)
import qualified Parse.Primitives.Internals as I
import qualified Parse.Primitives.Whitespace as W
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Syntax as E
import qualified Reporting.Region as R



-- RUN


run :: Parser a -> B.ByteString -> Either E.Error a
run parser bytes =
  runAt 1 1 parser bytes


runAt :: Int -> Int -> Parser a -> B.ByteString -> Either E.Error a
runAt startRow startColumn (Parser parser) (B.PS fp offset length) =
  case parser (State fp offset (offset + length) 0 startRow startColumn []) Ok Err Ok Err of
    Ok value _ _ ->
      Right value

    Err (E.ParseError row col problem) ->
      let
        pos = R.Position row col
        mkError overallRegion subRegion =
          Left (E.Parse overallRegion subRegion problem)
      in
        case problem of
          E.BadChar endCol ->
            mkError (R.Region pos (R.Position row endCol)) Nothing

          E.BadEscape width _ ->
            mkError (R.Region pos (R.Position row (col + width))) Nothing

          E.BadUnderscore badCol ->
            mkError (R.Region pos (R.Position row badCol)) Nothing

          E.BadOp _ ((_, start) : _) ->
            mkError (R.Region start pos) (Just (R.Region pos pos))

          E.Theories ((_, start) : _) _ ->
            mkError (R.Region start pos) (Just (R.Region pos pos))

          _ ->
            mkError (R.Region pos pos) Nothing



-- RESULT


data Result a
  = Ok a State E.ParseError
  | Err E.ParseError



-- COMBINATORS


try :: Parser a -> Parser a
try (Parser parser) =
  Parser $ \state cok _ eok eerr ->
    parser state cok eerr eok eerr


deadend :: [E.Theory] -> Parser a
deadend thrys =
  Parser $ \(State _ _ _ _ row col ctx) _ _ _ eerr ->
    eerr (E.ParseError row col (E.Theories ctx thrys))


hint :: E.Next -> Parser a -> Parser a
hint next (Parser parser) =
  Parser $ \state@(State _ _ _ _ row col ctx) cok cerr eok eerr ->
    let
      eok' x s _ =
        eok x s (expect row col ctx (E.Expecting next))

      eerr' _ =
        eerr (expect row col ctx (E.Expecting next))
    in
      parser state cok cerr eok' eerr'


endOfFile :: Parser ()
endOfFile =
  Parser $ \state@(State _ offset terminal _ _ _ _) _ _ eok eerr ->
    if offset < terminal then
      eerr noError
    else
      eok () state noError


noFloatsAllowedInPatterns :: Parser a
noFloatsAllowedInPatterns =
  Parser $ \(State _ _ _ _ row col _) _ cerr _ _ ->
    cerr (E.ParseError row col E.FloatInPattern)



-- STATE


{-# INLINE getPosition #-}
getPosition :: Parser R.Position
getPosition =
  Parser $ \state@(State _ _ _ _ row col _) _ _ eok _ ->
    eok (R.Position row col) state noError


getIndent :: Parser Int
getIndent =
  Parser $ \state@(State _ _ _ indent _ _ _) _ _ eok _ ->
    eok indent state noError


getCol :: Parser Int
getCol =
  Parser $ \state@(State _ _ _ _ _ col _) _ _ eok _ ->
    eok col state noError


pushContext :: R.Position -> E.Context -> Parser ()
pushContext pos ctx =
  Parser $ \state@(State _ _ _ _ _ _ context) _ _ eok _ ->
    eok () (state { _context = (ctx, pos) : context }) noError


popContext :: a -> Parser a
popContext value =
  Parser $ \state@(State _ _ _ _ _ _ context) _ _ eok _ ->
    eok value (state { _context = tail context }) noError


setIndent :: Int -> Parser ()
setIndent indent =
  Parser $ \state _ _ eok _ ->
    eok () (state { _indent = indent }) noError



-- SPACE PARSER


type SParser a =
  Parser (a, R.Position, W.SPos)



-- LOCATION


addLocation :: Parser a -> Parser (A.Located a)
addLocation parser =
  do  start <- getPosition
      value <- parser
      end <- getPosition
      return (A.at start end value)


inContext :: R.Position -> E.Context -> Parser a -> Parser a
inContext pos ctx parser =
  do  pushContext pos ctx
      a <- parser
      popContext a


-- WHITESPACE VARIATIONS


spaces :: Parser ()
spaces =
  checkSpace =<< W.whitespace


noSpace :: R.Position -> W.SPos -> Parser ()
noSpace pos (W.SPos spos) =
  if pos == spos
    then return ()
    else deadend []


checkSpace :: W.SPos -> Parser ()
checkSpace (W.SPos (R.Position _ col)) =
  do  indent <- getIndent
      if col > indent && col > 1
        then return ()
        else deadend [E.BadSpace]


checkAligned :: W.SPos -> Parser ()
checkAligned (W.SPos (R.Position _ col)) =
  do  indent <- getIndent
      if col == indent
        then return ()
        else deadend [E.BadSpace]


checkFreshLine :: W.SPos -> Parser ()
checkFreshLine (W.SPos (R.Position _ col)) =
  if col == 1
    then return ()
    else deadend [E.BadSpace]
