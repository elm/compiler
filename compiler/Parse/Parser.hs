module Parse.Parser (parseProgram, parseDependencies, parseInfix) where

import SourceSyntax.Module
import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.Char (isSymbol, isDigit)
import Data.List (foldl',intercalate)
import Text.Parsec hiding (newline,spaces)

import Parse.Helpers
import Parse.Binop (infixStmt, OpTable)
import Parse.Expression
import Parse.Type
import Parse.Module
import qualified Parse.Declaration as Decl

freshDef = commitIf (freshLine >> (letter <|> char '_')) $ do
             freshLine
             Decl.declaration <?> "another datatype or variable definition"

decls = do d <- Decl.declaration <?> "at least one datatype or variable definition"
           (d:) <$> many freshDef

program = do
  optional freshLine
  (names,exports) <- option (["Main"],[]) (moduleDef `followedBy` freshLine)
  is <- (do try (lookAhead $ reserved "import")
            imports `followedBy` freshLine) <|> return []
  declarations <- decls
  optional freshLine ; optional spaces ; eof
  return $ Module names exports is declarations

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
