module Parse.Parser (parseProgram, parseDependencies, parseInfix) where

import Ast
import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.Char (isSymbol, isDigit)
import Data.List (foldl',intercalate)
import Text.Parsec hiding (newline,spaces)

import Parse.Library
import Parse.Binops (infixStmt, OpTable)
import Parse.Expr
import Parse.Types
import Parse.Modules
import Parse.Foreign


statement = choice (typeAlias:defs) <|> def <?> "datatype or variable definition"
    where defs = map ((:[]) <$>) [ foreignDef, datatype ]

freshDef = commitIf (freshLine >> (letter <|> char '_')) $ do
             freshLine
             statement <?> "another datatype or variable definition"

defs1 = do d <- statement <?> "at least one datatype or variable definition"
           concat <$> (d:) <$> many freshDef

program = do
  optional freshLine
  (names,exports) <- option (["Main"],[]) (moduleDef `followedBy` freshLine)
  is <- (do try (lookAhead $ reserved "import")
            imports `followedBy` freshLine) <|> return []
  statements <- defs1
  optional freshLine ; optional spaces ; eof
  return $ Module names exports is statements

parseProgram = setupParser program

parseDependencies :: String -> Either String (String, [String])
parseDependencies =
    setupParser $ do
      optional skip
      (,) <$> option "Main" moduleName <*> option [] imprts
    where 
      skip = try (manyTill anyChar (try (string "/**")))
      imprts = fmap (map fst) imports `followedBy` freshLine
      getName = intercalate "." . fst
      moduleName = do optional freshLine 
                      getName <$> moduleDef `followedBy` freshLine

parseInfix :: String -> Either String OpTable
parseInfix = setupParser . many $ do
               manyTill (whitespace <|> (anyChar >> return ())) freshLine
               infixStmt

setupParser p source =
    case iParse p "" source of
      Right result -> Right result
      Left err -> Left $ "Parse error at " ++ show err
