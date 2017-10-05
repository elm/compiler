{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}
module Parse.Helpers
  ( module Parse.Primitives
  , SParser
  , qualifiedVar, qualifiedCapVar
  , equals, rightArrow, hasType, comma, pipe, cons, dot, minus, lambda
  , leftParen, rightParen, leftSquare, rightSquare, leftCurly, rightCurly
  , addLocation, inContext
  , spaces, noSpace, checkSpace, checkAligned, checkFreshLine
  )
  where

import qualified Data.Text as Text
import Data.Text (Text)

import Parse.Primitives
import qualified Parse.Primitives as P
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Syntax as E
import qualified Reporting.Region as R



-- SPACE PARSER


type SParser a =
  Parser (a, R.Position, SPos)



-- VARIABLES


qualifiedCapVar :: Parser Text
qualifiedCapVar =
  do  var <- capVar
      qualifiedVarHelp qualifiedCapVarHelp [var]


qualifiedCapVarHelp :: Parser a
qualifiedCapVarHelp =
  deadend [E.CapVar]


qualifiedVar :: Parser Text
qualifiedVar =
  oneOf
    [ lowVar
    , do  var <- capVar
          qualifiedVarHelp lowVar [var]
    ]


qualifiedVarHelp :: Parser Text -> [Text] -> Parser Text
qualifiedVarHelp altEnding vars =
  oneOf
    [ do  dot
          oneOf
            [ do  var <- capVar
                  qualifiedVarHelp altEnding (var:vars)
            , do  var <- altEnding
                  return (Text.intercalate "." (reverse (var:vars)))
            ]
    , return (Text.intercalate "." (reverse vars))
    ]



-- COMMON SYMBOLS


{-# NOINLINE equals #-}
equals :: Parser ()
equals =
  symbol "="


{-# NOINLINE rightArrow #-}
rightArrow :: Parser ()
rightArrow =
  symbol "->"


{-# NOINLINE hasType #-}
hasType :: Parser ()
hasType =
  symbol ":"


{-# NOINLINE comma #-}
comma :: Parser ()
comma =
  symbol ","


{-# NOINLINE pipe #-}
pipe :: Parser ()
pipe =
  symbol "|"


{-# NOINLINE cons #-}
cons :: Parser ()
cons =
  symbol "::"


{-# NOINLINE dot #-}
dot :: Parser ()
dot =
  symbol "."


{-# NOINLINE minus #-}
minus :: Parser ()
minus =
  symbol "-"


{-# NOINLINE lambda #-}
lambda :: Parser ()
lambda =
  oneOf [ symbol "\\", symbol "\x03BB" ]



-- ENCLOSURES


{-# NOINLINE leftParen #-}
leftParen :: Parser ()
leftParen =
  symbol "("


{-# NOINLINE rightParen #-}
rightParen :: Parser ()
rightParen =
  symbol ")"


{-# NOINLINE leftSquare #-}
leftSquare :: Parser ()
leftSquare =
  symbol "["


{-# NOINLINE rightSquare #-}
rightSquare :: Parser ()
rightSquare =
  symbol "]"


{-# NOINLINE leftCurly #-}
leftCurly :: Parser ()
leftCurly =
  symbol "{"


{-# NOINLINE rightCurly #-}
rightCurly :: Parser ()
rightCurly =
  symbol "}"



-- LOCATION


addLocation :: Parser a -> Parser (A.Located a)
addLocation parser =
  do  start <- getPosition
      value <- parser
      end <- getPosition
      return (A.at start end value)


inContext :: R.Position -> E.Context -> Parser a -> Parser a
inContext pos ctx parser =
  do  P.pushContext pos ctx
      a <- parser
      P.popContext a


-- WHITESPACE VARIATIONS


spaces :: Parser ()
spaces =
  checkSpace =<< whitespace


noSpace :: R.Position -> SPos -> Parser ()
noSpace pos (SPos spos) =
  if pos == spos
    then return ()
    else deadend []


checkSpace :: SPos -> Parser ()
checkSpace (SPos (R.Position _ col)) =
  do  indent <- getIndent
      if col > indent && col > 1
        then return ()
        else deadend [E.BadSpace]


checkAligned :: SPos -> Parser ()
checkAligned (SPos (R.Position _ col)) =
  do  indent <- getIndent
      if col == indent
        then return ()
        else deadend [E.BadSpace]


checkFreshLine :: SPos -> Parser ()
checkFreshLine (SPos (R.Position _ col)) =
  if col == 1
    then return ()
    else deadend [E.BadSpace]
