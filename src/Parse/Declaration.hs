{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
module Parse.Declaration where

import Text.Parsec ( (<|>), (<?>), choice, digit, optionMaybe, string, try )

import qualified AST.Declaration as D
import qualified Parse.Expression as Expr
import Parse.Helpers as Help
import qualified Parse.Type as Type



declaration :: IParser D.SourceDecl
declaration =
  choice
    [ D.Comment <$> Help.docComment
    , D.Decl <$> addLocation (typeDecl <|> infixDecl <|> definition)
    ]



-- TYPE ANNOTATIONS and DEFINITIONS


definition :: IParser D.SourceDecl'
definition =
  D.Definition
    <$> (Expr.typeAnnotation <|> Expr.definition)
    <?> "a value definition"



-- TYPE ALIAS and UNION TYPES


typeDecl :: IParser D.SourceDecl'
typeDecl =
  do  try (reserved "type") <?> "a type declaration"
      forcedWS
      isAlias <- optionMaybe (string "alias" >> forcedWS)

      name <- capVar
      args <- spacePrefix lowVar
      padded equals

      case isAlias of
        Just _ ->
            do  tipe <- Type.expr <?> "a type"
                return (D.TypeAlias name args tipe)

        Nothing ->
            do  tcs <- pipeSep1 Type.constructor <?> "a constructor for a union type"
                return $ D.Datatype name args tcs



-- INFIX


infixDecl :: IParser D.SourceDecl'
infixDecl =
  expecting "an infix declaration" $
  do  assoc <-
          choice
            [ try (reserved "infixl") >> return D.L
            , try (reserved "infixr") >> return D.R
            , try (reserved "infix")  >> return D.N
            ]
      forcedWS
      n <- digit
      forcedWS
      D.Fixity assoc (read [n]) <$> anyOp

