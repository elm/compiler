{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}
module Parse.Helpers
  ( module Parse.Primitives
  , SParser
  , qualifiedVar, qualifiedCapVar
  , equals, rightArrow, hasType, comma, pipe, cons, dot, minus, underscore, lambda
  , leftParen, rightParen, leftSquare, rightSquare, leftCurly, rightCurly
  , addLocation
  , spaces, checkSpace, checkAligned, checkFreshline
  )
  where

import qualified Data.Text as Text
import Data.Text (Text)

import Parse.Primitives
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
      qualifiedVarHelp True [var]


qualifiedVar :: Parser Text
qualifiedVar =
  oneOf
    [ lowVar
    , do  var <- capVar
          qualifiedVarHelp False [var]
    ]


qualifiedVarHelp :: Bool -> [Text] -> Parser Text
qualifiedVarHelp allCaps vars =
  oneOf
    [ do  dot
          oneOf
            [ do  var <- capVar
                  qualifiedVarHelp allCaps (var:vars)
            , if allCaps then
                failure E.BadChunkInQualifiedCapVar
              else
                do  var <- lowVar
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


{-# INLINE underscore #-}
underscore :: Parser ()
underscore =
  symbol "_"


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



-- WHITESPACE VARIATIONS


spaces :: Parser ()
spaces =
  do  (SPos (R.Position _ col)) <- whitespace
      indent <- getIndent
      if col > indent && col > 1
        then return ()
        else failure (E.Theories [E.BadIndent])


checkSpace :: SPos -> Parser ()
checkSpace (SPos (R.Position _ col)) =
  do  indent <- getIndent
      if col > indent && col > 1
        then return ()
        else deadend E.BadIndent


checkAligned :: SPos -> Parser ()
checkAligned (SPos (R.Position _ col)) =
  do  indent <- getIndent
      if col == indent
        then return ()
        else deadend E.BadIndent


checkFreshline :: SPos -> Parser ()
checkFreshline (SPos (R.Position _ col)) =
  if col == 1
    then return ()
    else deadend E.FreshLine
