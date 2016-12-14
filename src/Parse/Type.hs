{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}
module Parse.Type (expression, unionConstructor) where

import Data.Text (Text)

import qualified AST.Type as Type
import qualified AST.Variable as Var
import Parse.Helpers
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Syntax as E
import qualified Reporting.Region as R



-- TYPE TERMS


term :: Parser Type.Raw
term =
  hint E.Type $
    oneOf
      [ constructor
      , variable
      , tuple
      , record
      ]



-- TYPE VARIABLES


variable :: Parser Type.Raw
variable =
  addLocation $
    Type.RVar <$> lowVar



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
              rightArrow
              spaces
              (tipe2, end2, pos2) <- expression
              let tipe = A.at start end2 (Type.RLambda tipe1 tipe2)
              return ( tipe, end2, pos2 )
        , return ( tipe1, end1, pos1 )
        ]



-- TYPE CONSTRUCTORS


constructor :: Parser Type.Raw
constructor =
  addLocation $
    fmap (Type.RType . Var.Raw) qualifiedCapVar


app :: R.Position -> SParser Type.Raw
app start =
  do  ctor <- constructor
      ctorEnd <- getPosition
      ctorPos <- whitespace
      (args, end, pos) <- eatArgs [] ctorEnd ctorPos
      case args of
        [] ->
          return ( ctor, end, pos )

        _ ->
          let
            tipe =
              A.at start end (Type.RApp ctor args)
          in
            return ( tipe, end, pos )


unionConstructor :: SParser (Text, [Type.Raw])
unionConstructor =
  do  ctor <- capVar
      ctorEnd <- getPosition
      ctorSpace <- whitespace
      (args, end, space) <- eatArgs [] ctorEnd ctorSpace
      return ( (ctor, args), end, space )


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


tuple :: Parser Type.Raw
tuple =
  do  start <- getPosition
      leftParen
      inContext E.TypeTuple $
        oneOf
          [ do  rightParen
                end <- getPosition
                return (Type.tuple (R.Region start end) [])
          , do  spaces
                (tipe, _, pos) <- expression
                checkSpace pos
                tupleEnding start [tipe]
          ]


tupleEnding :: R.Position -> [Type.Raw] -> Parser Type.Raw
tupleEnding start tipes =
  oneOf
    [ do  comma
          spaces
          (tipe, _, pos) <- expression
          checkSpace pos
          tupleEnding start (tipe : tipes)
    , do  rightParen
          end <- getPosition
          case reverse tipes of
            [tipe] ->
              return tipe

            tupleTypes ->
              return (Type.tuple (R.Region start end) tupleTypes)
    ]



-- RECORD


record :: Parser Type.Raw
record =
  addLocation $
  do  leftCurly
      inContext E.TypeRecord $
        do  spaces
            oneOf
              [ do  rightCurly
                    return (Type.RRecord [] Nothing)
              , do  var <- addLocation lowVar
                    spaces
                    oneOf
                      [ do  pipe
                            spaces
                            firstField <- field
                            fields <- chompFields [firstField]
                            return (Type.RRecord fields (Just (A.map Type.RVar var)))
                      , do  hasType
                            spaces
                            (tipe, _, nextPos) <- expression
                            checkSpace nextPos
                            fields <- chompFields [(var, tipe)]
                            return (Type.RRecord fields Nothing)
                      ]
              ]


type Field = ( A.Located Text, Type.Raw )


chompFields :: [Field] -> Parser [Field]
chompFields fields =
  oneOf
    [ do  comma
          spaces
          f <- field
          chompFields (f : fields)
    , do  rightCurly
          return (reverse fields)
    ]


field :: Parser Field
field =
  do  name <- addLocation lowVar
      spaces
      hasType
      spaces
      (tipe, _, endPos) <- expression
      checkSpace endPos
      return (name, tipe)
