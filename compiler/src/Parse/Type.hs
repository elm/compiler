{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}
module Parse.Type
  ( expression
  , unionConstructor
  )
  where


import qualified Data.Name as Name

import qualified AST.Source as Src
import Parse.Primitives (addLocation, getPosition, inContext, oneOf, word1, word2)
import Parse.Utils (Parser, SParser, SPos, checkSpace, spaces, whitespace)
import qualified Parse.Variable as Var
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Syntax as E



-- TYPE TERMS


term :: Parser Src.Type
term =
  do  start <- getPosition
      oneOf E.XXX
        [
          -- types with no arguments (Int, Float, etc.)
          do  upper <- Var.foreignUpper
              end <- getPosition
              let region = A.Region start end
              return $ A.At region $
                case upper of
                  Var.Unqualified name ->
                    Src.TType region name []

                  Var.Qualified home name ->
                    Src.TTypeQual region home name []
        ,
          -- type variables
          do  var <- Var.lower
              end <- getPosition
              return (A.at start end (Src.TVar var))
        ,
          -- tuples
          inContext E.TypeTuple (word1 0x28 {-(-} E.XXX) $
            oneOf E.XXX
              [ do  word1 0x29 {-)-} E.XXX
                    end <- getPosition
                    return (A.at start end Src.TUnit)
              , do  spaces
                    (tipe, _, pos) <- expression
                    checkSpace pos
                    tupleEnding start tipe []
              ]
        ,
          -- records
          inContext E.TypeRecord (word1 0x7B {- { -} E.XXX) $
            do  spaces
                oneOf E.XXX
                  [ do  word1 0x7D {-}-} E.XXX
                        end <- getPosition
                        return (A.at start end (Src.TRecord [] Nothing))
                  , do  var <- addLocation Var.lower
                        spaces
                        oneOf E.XXX
                          [ do  word1 0x7C {-|-} E.XXX
                                spaces
                                firstField <- field
                                fields <- chompFields [firstField]
                                end <- getPosition
                                return (A.at start end (Src.TRecord fields (Just var)))
                          , do  word1 0x3A {-:-} E.XXX
                                spaces
                                (tipe, _, nextPos) <- expression
                                checkSpace nextPos
                                fields <- chompFields [(var, tipe)]
                                end <- getPosition
                                return (A.at start end (Src.TRecord fields Nothing))
                          ]
                  ]
        ]



-- TYPE EXPRESSIONS


expression :: SParser Src.Type
expression =
  do  start <- getPosition
      (tipe1, end1, pos1) <-
        oneOf E.XXX
          [ app start
          , (,,) <$> term <*> getPosition <*> whitespace
          ]
      oneOf E.XXX
        [ do  checkSpace pos1
              word2 0x2D 0x3E {-->-} E.XXX
              spaces
              (tipe2, end2, pos2) <- expression
              let tipe = A.at start end2 (Src.TLambda tipe1 tipe2)
              return ( tipe, end2, pos2 )
        , return ( tipe1, end1, pos1 )
        ]



-- TYPE CONSTRUCTORS


app :: A.Position -> SParser Src.Type
app start =
  do  upper <- Var.foreignUpper
      nameEnd <- getPosition
      namePos <- whitespace
      (args, end, pos) <- eatArgs [] nameEnd namePos

      let region = A.Region start nameEnd
      let tipe =
            case upper of
              Var.Unqualified name ->
                Src.TType region name args

              Var.Qualified home name ->
                Src.TTypeQual region home name args

      return ( A.at start end tipe, end, pos )


unionConstructor :: SParser (A.Located Name.Name, [Src.Type])
unionConstructor =
  do  start <- getPosition
      name <- Var.upper
      nameEnd <- getPosition
      namePos <- whitespace
      (args, end, pos) <- eatArgs [] nameEnd namePos
      return ( (A.at start nameEnd name, args), end, pos )


eatArgs :: [Src.Type] -> A.Position -> SPos -> SParser [Src.Type]
eatArgs args end pos =
  oneOf E.XXX
    [ do  checkSpace pos
          arg <- term
          newEnd <- getPosition
          newSpace <- whitespace
          eatArgs (arg:args) newEnd newSpace
    , return ( reverse args, end, pos )
    ]



-- TUPLES


tupleEnding :: A.Position -> Src.Type -> [Src.Type] -> Parser Src.Type
tupleEnding start firstType revTypes =
  oneOf E.XXX
    [ do  word1 0x2C {-,-} E.XXX
          spaces
          (tipe, _, pos) <- expression
          checkSpace pos
          tupleEnding start firstType (tipe : revTypes)
    , do  word1 0x29 {-)-} E.XXX
          end <- getPosition
          case reverse revTypes of
            [] ->
              return firstType

            secondType : otherTypes ->
              return $ A.at start end $
                Src.TTuple firstType secondType otherTypes
    ]



-- RECORD


type Field = ( A.Located Name.Name, Src.Type )


chompFields :: [Field] -> Parser [Field]
chompFields fields =
  oneOf E.XXX
    [ do  word1 0x2C {-,-} E.XXX
          spaces
          f <- field
          chompFields (f : fields)
    , do  word1 0x7D {-}-} E.XXX
          return (reverse fields)
    ]


field :: Parser Field
field =
  do  name <- addLocation Var.lower
      spaces
      word1 0x3A {-:-} E.XXX
      spaces
      (tipe, _, endPos) <- expression
      checkSpace endPos
      return (name, tipe)
