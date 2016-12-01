{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}
module Parse.Type (expression, unionConstructor) where

import Data.Text (Text)

import qualified AST.Type as Type
import qualified AST.Variable as Var
import Parse.Helpers
import qualified Reporting.Annotation as A
import qualified Reporting.Region as R



-- TYPE TERMS


term :: Parser Type.Raw
term =
  oneOf
    [ constructor
    , variable
    , tuple
    , record
    ]



-- TYPE VARIABLES


variable :: Parser Type.Raw
variable =
  expecting "a type variable" $ addLocation $
    Type.RVar <$> lowVar



-- TYPE EXPRESSIONS


expression :: SParser Type.Raw
expression =
  do  start <- getPosition
      (tipe1, end1, space1) <-
        oneOf
          [ app start
          , (,,) <$> term <*> getPosition <*> whitespace
          ]
      oneOf
        [ do  checkSpaces space1
              rightArrow
              spaces
              (tipe2, end2, space2) <- expression
              let tipe = A.at start end2 (Type.RLambda tipe1 tipe2)
              return ( tipe, end2, space2 )
        , return ( tipe1, end1, space1 )
        ]



-- TYPE CONSTRUCTORS


constructor :: Parser Type.Raw
constructor =
  expecting "a type constructor, like Maybe or List" $ addLocation $
    fmap (Type.RType . Var.Raw) qualifiedCapVar


app :: R.Position -> SParser Type.Raw
app start =
  do  ctor <- constructor
      ctorEnd <- getPosition
      ctorSpace <- whitespace
      (args, end, space) <- eatArgs [] ctorEnd ctorSpace
      case args of
        [] ->
          return ( ctor, end, space )

        _ ->
          let
            tipe =
              A.at start end (Type.RApp ctor (reverse args))
          in
            return ( tipe, end, space )


unionConstructor :: SParser (Text, [Type.Raw])
unionConstructor =
  do  ctor <- capVar
      ctorEnd <- getPosition
      ctorSpace <- whitespace
      (args, end, space) <- eatArgs [] ctorEnd ctorSpace
      return ( (ctor, args), end, space )


eatArgs :: [Type.Raw] -> R.Position -> Space -> SParser [Type.Raw]
eatArgs args end space =
  oneOf
    [ do  checkSpaces space
          arg <- term
          newEnd <- getPosition
          newSpace <- whitespace
          eatArgs (arg:args) newEnd newSpace
    , return ( reverse args, end, space )
    ]



-- TUPLES


tuple :: Parser Type.Raw
tuple =
  do  start <- getPosition
      leftParen
      spaces
      tupleEnding start []


tupleEnding :: R.Position -> [Type.Raw] -> Parser Type.Raw
tupleEnding start tipes =
  oneOf
    [ do  comma
          spaces
          (tipe, _, space) <- expression
          checkSpaces space
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
      spaces
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
                      (tipe, _, nextSpace) <- expression
                      checkSpaces nextSpace
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
          return fields
    ]


field :: Parser Field
field =
  do  name <- addLocation lowVar
      spaces
      hasType
      spaces
      (tipe, _, endSpace) <- expression
      checkSpaces endSpace
      return (name, tipe)
