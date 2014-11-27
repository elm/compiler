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
    typeDecl <|> infixDecl <|> port <|> definition


definition :: IParser D.SourceDecl
definition = D.Definition <$> Expr.def


typeDecl :: IParser D.SourceDecl
typeDecl =
 do reserved "type" <?> "type declaration"
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
infixDecl = do
  assoc <- choice [ reserved "infixl" >> return D.L
                  , reserved "infix"  >> return D.N
                  , reserved "infixr" >> return D.R ]
  forcedWS
  n <- digit
  forcedWS
  D.Fixity assoc (read [n]) <$> anyOp


port :: IParser D.SourceDecl
port =
  do try (reserved "port")
     whitespace
     name <- lowVar
     whitespace
     let port' op ctor expr = do { try op ; whitespace ; ctor name <$> expr }
     D.Port <$> choice [ port' hasType D.PPAnnotation Type.expr
                       , port' equals  D.PPDef Expr.expr ]
