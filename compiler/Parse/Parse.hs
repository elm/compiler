module Parse.Parser (parseProgram) where

import SourceSyntax.Module
import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.Char (isSymbol, isDigit)
import Data.List (foldl',intercalate)
import Text.Parsec hiding (newline,spaces)
import qualified Text.PrettyPrint as P

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

parseProgram :: String -> Either [P.Doc] (Module t v)
parseProgram = setupParser program

setupParser :: IParser a -> String -> Either [P.Doc] a
setupParser p source =
    case iParse p "" source of
      Right result -> Right result
      Left err -> Left [ P.sep . map P.text . words $ "Parse error at " ++ show err ]
