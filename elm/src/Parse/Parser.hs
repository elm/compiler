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


freshDef = commitIf (freshLine >> (letter <|> char '_')) $ do
             freshLine
             datatype <|> def <?> "another datatype or variable definition"

defs1 = do d <- datatype <|> def <?> "at least one datatype or variable definition"
           (d:) <$> many freshDef

defs = do
  (fss,ess,tss) <- unzip3 <$> defs1
  return (concat fss, concat ess, concat `liftM` sequence tss)

program = do
  optional freshLine
  (names,exports) <- option ([],[]) (moduleDef `followedBy` freshLine)
  is <- (do try (lookAhead $ reserved "import")
            imports `followedBy` freshLine) <|> return []
  jsffi <- foreignDefs
  freshLine
  (vs,es,ts) <- defs
  optional freshLine ; optional spaces ; eof
  return (Module names exports is (zip vs es) jsffi, zip vs `liftM` ts)

parseProgram source = 
    case parse program "" source of
      Right result -> Right result
      Left err -> Left $ "Parse error at " ++ show err
