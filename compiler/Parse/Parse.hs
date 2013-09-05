module Parse.Parse (program, dependencies) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.Char (isSymbol, isDigit)
import Data.List (foldl',intercalate)
import Text.Parsec hiding (newline,spaces)
import qualified Text.PrettyPrint as P

import qualified SourceSyntax.Module as S
import Parse.Helpers
import Parse.Expression
import Parse.Type
import Parse.Module
import qualified Parse.Declaration as Decl

freshDef = commitIf (freshLine >> (letter <|> char '_')) $ do
             freshLine
             Decl.declaration <?> "another datatype or variable definition"

decls = do d <- Decl.declaration <?> "at least one datatype or variable definition"
           (d:) <$> many freshDef

program :: String -> Either [P.Doc] (S.Module t v)
program = setupParser $ do
  optional freshLine
  (names,exports) <- option (["Main"],[]) (moduleDef `followedBy` freshLine)
  is <- (do try (lookAhead $ reserved "import")
            imports `followedBy` freshLine) <|> return []
  declarations <- decls
  optional freshLine ; optional spaces ; eof
  return $ S.Module names exports is declarations

dependencies :: String -> Either [P.Doc] (String, [String])
dependencies =
    let getName = intercalate "." . fst in
    setupParser $ do
      optional freshLine
      (,) <$> option "Main" (getName <$> moduleDef `followedBy` freshLine)
          <*> option [] (map fst <$> imports `followedBy` freshLine)

setupParser :: IParser a -> String -> Either [P.Doc] a
setupParser p source =
    case iParse p "" source of
      Right result -> Right result
      Left err -> Left [ P.text $ "Parse error at " ++ show err ]
