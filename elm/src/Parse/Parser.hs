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



defs1 = do d <- datatype <|> def <?> "at least one definition"
           (d:) <$> many (try (try freshLine >> (datatype <|> def)))

defs = do
  (fss,ess,tss) <- unzip3 <$> defs1
  return (concat fss, concat ess, concat `liftM` sequence tss)


program = do
  optional freshLine
  (name,exports) <- option ("",[]) moduleDef
  freshLine
  is <- imports
  freshLine
  ds <- defs
  optional freshLine ; optional spaces ; eof
  return (Module name exports is, ds)

parseProgram source = 
    case parse program "" source of
      Right result -> Right result
      Left err -> Left $ "Parse error at " ++ show err
