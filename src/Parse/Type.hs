{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}
module Parse.Type (expression, unionConstructor) where

import Data.Text (Text)

import qualified AST.Type as Type
import Parse.Primitives (Parser, SParser, addLocation, checkSpace, getPosition, hint, spaces, oneOf)
import qualified Parse.Primitives as P
import qualified Parse.Primitives.Symbol as Symbol
import qualified Parse.Primitives.Variable as Var
import Parse.Primitives.Whitespace (SPos, whitespace)

import qualified Reporting.Annotation as A
import qualified Reporting.Error.Syntax as E
import qualified Reporting.Region as R



-- TYPE TERMS


term :: Parser Type.Raw
term =
  hint E.Type $
    do  start <- getPosition
        oneOf
          [ app0
          , variable start
          , tuple start
          , record start
          ]



-- TYPE VARIABLES


variable :: R.Position -> Parser Type.Raw
variable start =
  do  var <- Var.lower
      end <- getPosition
      return (A.at start end (Type.RVar var))



-- TYPE EXPRESSIONS


expression :: SParser Type.Raw
expression =
  hint E.Type $
  do  start <- getPosition
      (tipe1, end1, pos1) <-
        oneOf
          [ app start
          , (,,) <$> term <*> getPosition <*> whitespace
          ]
      oneOf
        [ do  checkSpace pos1
              Symbol.rightArrow
              spaces
              (tipe2, end2, pos2) <- expression
              let tipe = A.at start end2 (Type.RLambda tipe1 tipe2)
              return ( tipe, end2, pos2 )
        , return ( tipe1, end1, pos1 )
        ]



-- TYPE CONSTRUCTORS


app0 :: Parser Type.Raw
app0 =
  do  start <- getPosition
      (maybeQualifier, name) <- Var.foreignUpper
      end <- getPosition
      return $ A.at start end $
        Type.RType (R.Region start end) maybeQualifier name []


app :: R.Position -> SParser Type.Raw
app start =
  do  (maybeQualifier, name) <- Var.foreignUpper
      nameEnd <- getPosition
      namePos <- whitespace
      (args, end, pos) <- eatArgs [] nameEnd namePos
      let tipe = Type.RType (R.Region start nameEnd) maybeQualifier name args
      return ( A.at start end tipe, end, pos )


unionConstructor :: SParser (A.Located Text, [Type.Raw])
unionConstructor =
  do  start <- getPosition
      name <- Var.upper
      nameEnd <- getPosition
      namePos <- whitespace
      (args, end, pos) <- eatArgs [] nameEnd namePos
      return ( (A.at start nameEnd name, args), end, pos )


eatArgs :: [Type.Raw] -> R.Position -> SPos -> SParser [Type.Raw]
eatArgs args end pos =
  oneOf
    [ do  checkSpace pos
          arg <- term
          newEnd <- getPosition
          newSpace <- whitespace
          eatArgs (arg:args) newEnd newSpace
    , return ( reverse args, end, pos )
    ]



-- TUPLES


tuple :: R.Position -> Parser Type.Raw
tuple start =
  do  Symbol.leftParen
      P.inContext start E.TypeTuple $
        oneOf
          [ do  Symbol.rightParen
                end <- getPosition
                return (A.at start end Type.RUnit)
          , do  spaces
                (tipe, _, pos) <- expression
                checkSpace pos
                tupleEnding start tipe []
          ]


tupleEnding :: R.Position -> Type.Raw -> [Type.Raw] -> Parser Type.Raw
tupleEnding start firstType revTypes =
  oneOf
    [ do  Symbol.comma
          spaces
          (tipe, _, pos) <- expression
          checkSpace pos
          tupleEnding start firstType (tipe : revTypes)
    , do  Symbol.rightParen
          end <- getPosition
          case reverse revTypes of
            [] ->
              return firstType

            secondType : otherTypes ->
              return $ A.at start end $
                Type.RTuple firstType secondType otherTypes
    ]



-- RECORD


record :: R.Position -> Parser Type.Raw
record start =
  do  Symbol.leftCurly
      P.inContext start E.TypeRecord $
        do  spaces
            oneOf
              [ do  Symbol.rightCurly
                    end <- getPosition
                    return (A.at start end (Type.RRecord [] Nothing))
              , do  var <- addLocation Var.lower
                    spaces
                    oneOf
                      [ do  Symbol.pipe
                            spaces
                            firstField <- field
                            fields <- chompFields [firstField]
                            end <- getPosition
                            return (A.at start end (Type.RRecord fields (Just (A.map Type.RVar var))))
                      , do  Symbol.hasType
                            spaces
                            (tipe, _, nextPos) <- expression
                            checkSpace nextPos
                            fields <- chompFields [(var, tipe)]
                            end <- getPosition
                            return (A.at start end (Type.RRecord fields Nothing))
                      ]
              ]


type Field = ( A.Located Text, Type.Raw )


chompFields :: [Field] -> Parser [Field]
chompFields fields =
  oneOf
    [ do  Symbol.comma
          spaces
          f <- field
          chompFields (f : fields)
    , do  Symbol.rightCurly
          return (reverse fields)
    ]


field :: Parser Field
field =
  do  name <- addLocation Var.lower
      spaces
      Symbol.hasType
      spaces
      (tipe, _, endPos) <- expression
      checkSpace endPos
      return (name, tipe)
