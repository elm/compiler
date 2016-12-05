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

import Parse.Primitives hiding (text)
import qualified Parse.Primitives as Prim
import qualified Reporting.Annotation as A
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
                failure False -- TODO
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
  expecting "an equals sign '='" $
    Prim.text "="


{-# INLINE rightArrow #-}
rightArrow :: Parser ()
rightArrow =
  expecting "an arrow '->'" $
    Prim.text "->"


{-# INLINE hasType #-}
hasType :: Parser ()
hasType =
  expecting "the \"has type\" symbol ':'" $
    Prim.text ":"


{-# INLINE comma #-}
comma :: Parser ()
comma =
  expecting "a comma ','" $
    Prim.text ","


{-# INLINE pipe #-}
pipe :: Parser ()
pipe =
  expecting "a vertical bar '|'" $
    Prim.text "|"


{-# INLINE cons #-}
cons :: Parser ()
cons =
  expecting "a cons operator '::'" $
    Prim.text "::"


{-# INLINE dot #-}
dot :: Parser ()
dot =
  expecting "a dot '.'" $
    Prim.text "."


{-# INLINE minus #-}
minus :: Parser ()
minus =
  Prim.text "-"


{-# INLINE underscore #-}
underscore :: Parser ()
underscore =
  expecting "a wildcard '_'" $
    Prim.text "_"


{-# INLINE lambda #-}
lambda :: Parser ()
lambda =
  oneOf [ Prim.text "\\", Prim.text "\x03BB" ]



-- ENCLOSURES


{-# INLINE leftParen #-}
leftParen :: Parser ()
leftParen =
  Prim.text "("


{-# INLINE rightParen #-}
rightParen :: Parser ()
rightParen =
  Prim.text ")"


{-# INLINE leftSquare #-}
leftSquare :: Parser ()
leftSquare =
  Prim.text "["


{-# INLINE rightSquare #-}
rightSquare :: Parser ()
rightSquare =
  Prim.text "]"


{-# INLINE leftCurly #-}
leftCurly :: Parser ()
leftCurly =
  Prim.text "{"


{-# INLINE rightCurly #-}
rightCurly :: Parser ()
rightCurly =
  Prim.text "}"





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
        else failure False -- TODO


checkSpace :: SPos -> Parser ()
checkSpace (SPos (R.Position _ col)) =
  do  indent <- getIndent
      if col > indent && col > 1
        then return ()
        else deadend False -- TODO


checkAligned :: SPos -> Parser ()
checkAligned (SPos (R.Position _ col)) =
  do  indent <- getIndent
      if col == indent
        then return ()
        else deadend False -- TODO


checkFreshline :: SPos -> Parser ()
checkFreshline (SPos (R.Position _ col)) =
  if col == 1
    then return ()
    else deadend False -- TODO
