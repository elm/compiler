
module Parse.Declaration where

import Control.Applicative ((<$>), (<*>))
import qualified Data.List as List
import qualified Data.Set as Set
import Text.Parsec hiding (newline,spaces)
import Text.Parsec.Indent

import Parse.Helpers
import qualified Parse.Expression as Expr
import qualified SourceSyntax.Type as T
import qualified Parse.Type as Type
import qualified SourceSyntax.Declaration as D


declaration :: IParser (D.Declaration t v)
declaration = alias <|> datatype <|> infixDecl <|> port <|> definition

definition :: IParser (D.Declaration t v)
definition = D.Definition <$> Expr.def

alias :: IParser (D.Declaration t v)
alias = do
  reserved "type" <?> "type alias (type Point = {x:Int, y:Int})"
  forcedWS
  alias <- capVar
  args  <- spacePrefix lowVar
  padded equals
  tipe <- Type.expr
  json <- option [] $ do
            try $ padded (reserved "deriving")
            string "Json"
            return [D.Json]
  return (D.TypeAlias alias args tipe json)

datatype :: IParser (D.Declaration t v)
datatype = do
  reserved "data" <?> "datatype definition (data T = A | B | ...)"
  forcedWS
  name <- capVar <?> "name of data-type"
  args <- spacePrefix lowVar
  padded equals
  tcs <- pipeSep1 Type.constructor
  return $ D.Datatype name args tcs []


infixDecl :: IParser (D.Declaration t v)
infixDecl = do
  assoc <- choice [ reserved "infixl" >> return D.L
                  , reserved "infix"  >> return D.N
                  , reserved "infixr" >> return D.R ]
  forcedWS
  n <- digit
  forcedWS
  D.Fixity assoc (read [n]) <$> anyOp


port :: IParser (D.Declaration t v)
port =
  do try (reserved "port")
     whitespace
     name <- lowVar
     padded hasType
     tipe <- Type.expr
     expr <- choice [ do try (outPort name)
                         padded equals
                         Just <$> Expr.expr
                    , return Nothing
                    ]
     return $ D.Port name tipe expr
  where
    outPort name = do
         freshLine
         reserved "port"
         whitespace
         name' <- lowVar
         if name == name' then return () else fail "different port"