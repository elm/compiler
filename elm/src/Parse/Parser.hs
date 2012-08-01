module Parser (parseProgram) where

import Ast
import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.Char (isSymbol, isDigit)
import Data.List (foldl')
import Text.Parsec hiding (newline,spaces)

import ParseLib
import ParseExpr
import ParseTypes
import ParseModules
import ParseForeign


statement =  (:[]) <$> foreignDef
         <|> (:[]) <$> datatype
         <|> def
         <?> "datatype or variable definition"

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

parseProgram source = 
    case parse program "" source of
      Right result -> Right result
      Left err -> Left $ "Parse error at " ++ show err
