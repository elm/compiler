{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
module Parse.Declaration where

import Control.Applicative ((<$>))
import Text.Parsec
    ( (<|>), (<?>), choice, digit, getPosition
    , optionMaybe, sourceLine, string, try
    )

import qualified AST.Declaration as D
import qualified Parse.Expression as Expr
import Parse.Helpers
import qualified Parse.Type as Type


declaration :: IParser D.SourceDecl
declaration =
  typeDecl <|> infixDecl <|> port <|> definition <|> perform


definition :: IParser D.SourceDecl
definition =
  D.Definition <$> Expr.def


typeDecl :: IParser D.SourceDecl
typeDecl =
  do  try (reserved "type") <?> "type declaration"
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


infixDecl :: IParser D.SourceDecl
infixDecl =
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


port :: IParser D.SourceDecl
port =
  do  try (reserved "port")
      whitespace
      name <- lowVar
      whitespace
      tipe <- Type.expr <?> "a type"
      return (D.Port name tipe)


perform :: IParser D.SourceDecl
perform =
  do  try (reserved "perform")
      line <- sourceLine <$> getPosition
      whitespace
      D.Perform line <$> Expr.expr

