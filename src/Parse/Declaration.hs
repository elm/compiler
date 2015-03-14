{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
module Parse.Declaration where

import Control.Applicative ((<$>))
import Text.Parsec ((<|>), (<?>), choice, digit, optionMaybe, string, try)

import qualified AST.Declaration as D
import qualified Parse.Expression as Expr
import Parse.Helpers
import qualified Parse.Type as Type


declaration :: IParser D.SourceDecl
declaration =
  typeDecl <|> infixDecl <|> foreign <|> input <|> definition


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


foreign :: IParser D.SourceDecl
foreign =
  do  try (reserved "foreign")
      whitespace
      choice
        [ do  name <- wireBegining "input"
              wireEnding (D.InputAnnotation name) hasType Type.expr
        , do  name <- wireBegining "output"
              choice
                [ wireEnding (D.OutputAnnotation name) hasType Type.expr
                , wireEnding (D.OutputDefinition name) equals Expr.expr
                ]
        ]


input :: IParser D.SourceDecl
input =
  do  name <- wireBegining "input"
      choice
        [ wireEnding (D.LoopbackAnnotation name) hasType Type.expr
        , wireEnding (D.LoopbackDefinition name) (string "from") Expr.expr
        ]


wireBegining :: String -> IParser String
wireBegining keyword =
  do  try (reserved keyword)
      whitespace
      name <- lowVar
      whitespace
      return name


wireEnding :: (a -> D.RawWire) -> IParser String -> IParser a -> IParser D.SourceDecl
wireEnding makeWire op valueParser =
  do  op
      whitespace
      value <- valueParser
      return (D.Wire (makeWire value))
