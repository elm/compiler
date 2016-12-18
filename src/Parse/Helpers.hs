{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}
module Parse.Helpers
  ( module Parse.Primitives
  , SParser
  , qualifiedVar, qualifiedCapVar
  , equals, rightArrow, hasType, comma, pipe, cons, dot, minus, lambda
  , leftParen, rightParen, leftSquare, rightSquare, leftCurly, rightCurly
  , addLocation, inContext
  , spaces, checkSpace, checkAligned, checkFreshLine
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


{-# INLINE equals #-}
equals :: Parser ()
equals =
  symbol "="


{-# INLINE rightArrow #-}
rightArrow :: Parser ()
rightArrow =
  symbol "->"


{-# INLINE hasType #-}
hasType :: Parser ()
hasType =
  symbol ":"


{-# INLINE comma #-}
comma :: Parser ()
comma =
  symbol ","


{-# INLINE pipe #-}
pipe :: Parser ()
pipe =
  symbol "|"


{-# INLINE cons #-}
cons :: Parser ()
cons =
  symbol "::"


{-# INLINE dot #-}
dot :: Parser ()
dot =
  symbol "."


{-# INLINE minus #-}
minus :: Parser ()
minus =
  symbol "-"


{-# INLINE lambda #-}
lambda :: Parser ()
lambda =
  oneOf [ symbol "\\", symbol "\x03BB" ]



-- ENCLOSURES


{-# INLINE leftParen #-}
leftParen :: Parser ()
leftParen =
  symbol "("


{-# INLINE rightParen #-}
rightParen :: Parser ()
rightParen =
  symbol ")"


{-# INLINE leftSquare #-}
leftSquare :: Parser ()
leftSquare =
  symbol "["


{-# INLINE rightSquare #-}
rightSquare :: Parser ()
rightSquare =
  symbol "]"


{-# INLINE leftCurly #-}
leftCurly :: Parser ()
leftCurly =
  symbol "{"


{-# INLINE rightCurly #-}
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
