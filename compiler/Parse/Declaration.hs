{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
module Parse.Declaration where

import Control.Applicative ((<$>))
import Text.Parsec hiding (newline,spaces)

import Parse.Helpers
import qualified Parse.Expression as Expr
import qualified Parse.Type as Type
import qualified SourceSyntax.Declaration as D


declaration :: IParser D.ParseDeclaration
declaration = alias <|> datatype <|> infixDecl <|> port <|> definition

definition :: IParser D.ParseDeclaration
definition = D.Definition <$> Expr.def

alias :: IParser D.ParseDeclaration
alias = do
  reserved "type" <?> "type alias (type Point = {x:Int, y:Int})"
  forcedWS
  name <- capVar
  args <- spacePrefix lowVar
  padded equals
  tipe <- Type.expr
  json <- option [] $ do
            try $ padded (reserved "deriving")
            string "Json"
            return [D.Json]
  return (D.TypeAlias name args tipe json)

datatype :: IParser D.ParseDeclaration
datatype = do
  reserved "data" <?> "datatype definition (data T = A | B | ...)"
  forcedWS
  name <- capVar <?> "name of data-type"
  args <- spacePrefix lowVar
  padded equals
  tcs <- pipeSep1 Type.constructor
  return $ D.Datatype name args tcs []


infixDecl :: IParser D.ParseDeclaration
infixDecl = do
  assoc <- choice [ reserved "infixl" >> return D.L
                  , reserved "infix"  >> return D.N
                  , reserved "infixr" >> return D.R ]
  forcedWS
  n <- digit
  forcedWS
  D.Fixity assoc (read [n]) <$> anyOp


port :: IParser D.ParseDeclaration
port =
  do try (reserved "port")
     whitespace
     name <- lowVar
     whitespace
     let port' op ctor expr = do { try op ; whitespace ; ctor name <$> expr }
     D.Port <$> choice [ port' hasType D.PPAnnotation Type.expr
                       , port' equals  D.PPDef Expr.expr ]