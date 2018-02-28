{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}
module Parse.Type
  ( expression
  , unionConstructor
  )
  where


import qualified AST.Source as Src
import qualified Elm.Name as N
import Parse.Primitives (Parser, SParser, addLocation, checkSpace, getPosition, hint, spaces, oneOf)
import qualified Parse.Primitives as P
import qualified Parse.Primitives.Symbol as Symbol
import qualified Parse.Primitives.Variable as Var
import Parse.Primitives.Whitespace (SPos, whitespace)
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Syntax as E
import qualified Reporting.Region as R



-- TYPE TERMS


term :: Parser Src.Type
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


variable :: R.Position -> Parser Src.Type
variable start =
  do  var <- Var.lower
      end <- getPosition
      return (A.at start end (Src.TVar var))



-- TYPE EXPRESSIONS


expression :: SParser Src.Type
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
              let tipe = A.at start end2 (Src.TLambda tipe1 tipe2)
              return ( tipe, end2, pos2 )
        , return ( tipe1, end1, pos1 )
        ]



-- TYPE CONSTRUCTORS


app0 :: Parser Src.Type
app0 =
  do  start <- getPosition
      upper <- Var.foreignUpper
      end <- getPosition
      let region = R.Region start end
      return $ A.At region $
        case upper of
          Var.Unqualified name ->
            Src.TType region name []

          Var.Qualified home name ->
            Src.TTypeQual region home name []


app :: R.Position -> SParser Src.Type
app start =
  do  upper <- Var.foreignUpper
      nameEnd <- getPosition
      namePos <- whitespace
      (args, end, pos) <- eatArgs [] nameEnd namePos

      let region = R.Region start nameEnd
      let tipe =
            case upper of
              Var.Unqualified name ->
                Src.TType region name args

              Var.Qualified home name ->
                Src.TTypeQual region home name args

      return ( A.at start end tipe, end, pos )


unionConstructor :: SParser (A.Located N.Name, [Src.Type])
unionConstructor =
  do  start <- getPosition
      name <- Var.upper
      nameEnd <- getPosition
      namePos <- whitespace
      (args, end, pos) <- eatArgs [] nameEnd namePos
      return ( (A.at start nameEnd name, args), end, pos )


eatArgs :: [Src.Type] -> R.Position -> SPos -> SParser [Src.Type]
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


tuple :: R.Position -> Parser Src.Type
tuple start =
  do  Symbol.leftParen
      P.inContext start E.TypeTuple $
        oneOf
          [ do  Symbol.rightParen
                end <- getPosition
                return (A.at start end Src.TUnit)
          , do  spaces
                (tipe, _, pos) <- expression
                checkSpace pos
                tupleEnding start tipe []
          ]


tupleEnding :: R.Position -> Src.Type -> [Src.Type] -> Parser Src.Type
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
                Src.TTuple firstType secondType otherTypes
    ]



-- RECORD


record :: R.Position -> Parser Src.Type
record start =
  do  Symbol.leftCurly
      P.inContext start E.TypeRecord $
        do  spaces
            oneOf
              [ do  Symbol.rightCurly
                    end <- getPosition
                    return (A.at start end (Src.TRecord [] Nothing))
              , do  var <- addLocation Var.lower
                    spaces
                    oneOf
                      [ do  Symbol.pipe
                            spaces
                            firstField <- field
                            fields <- chompFields [firstField]
                            end <- getPosition
                            return (A.at start end (Src.TRecord fields (Just var)))
                      , do  Symbol.hasType
                            spaces
                            (tipe, _, nextPos) <- expression
                            checkSpace nextPos
                            fields <- chompFields [(var, tipe)]
                            end <- getPosition
                            return (A.at start end (Src.TRecord fields Nothing))
                      ]
              ]


type Field = ( A.Located N.Name, Src.Type )


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
