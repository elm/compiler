module Parser (toDefs) where

import Ast
import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.Char (isSymbol, isDigit)
import Data.List (foldl')
import Text.Parsec hiding (newline,spaces)

import ParseLib
import Patterns
import Binops
import ParseTypes

import Guid
import Types (Type (VarT))

--------  Basic Terms  --------

numTerm :: (Monad m) => ParsecT [Char] u m Expr
numTerm = liftM (Number . read) (many1 digit) <?> "number"

strTerm :: (Monad m) => ParsecT [Char] u m Expr
strTerm = liftM Str . expecting "string" . betwixt '"' '"' . many $
          backslashed <|> satisfy (/='"')

varTerm :: (Monad m) => ParsecT [Char] u m Expr
varTerm = toVar <$> var <?> "variable"

toVar v = case v of "True"  -> Boolean True
                    "False" -> Boolean False
                    _       -> Var v

chrTerm :: (Monad m) => ParsecT [Char] u m Expr
chrTerm = Chr <$> betwixt '\'' '\'' (backslashed <|> satisfy (/='\'')) <?> "character"


--------  Complex Terms  --------

listTerm = braces $ choice
           [ try $ do { lo <- expr; whitespace; string ".." ; whitespace
                      ; Range lo <$> expr }
           , list <$> commaSep expr
           ]

parensTerm = parens $ choice
             [ do op <- anyOp
                  return . Lambda "x" . Lambda "y" $ Binop op (Var "x") (Var "y")
             , do es <- commaSep expr
                  return $ case es of { [e] -> e; _ -> tuple es }
             ]

term = choice [ numTerm, strTerm, chrTerm
              , accessible varTerm
              , listTerm, parensTerm ]
       <?> "basic term (number, variable, etc.)"

--------  Applications  --------

appExpr = do
  tlist <- spaceSep1 term
  return $ case tlist of
             t:[] -> t
             t:ts -> foldl' App t ts

--------  Normal Expressions  --------

binaryExpr = binops appExpr anyOp

ifExpr = do string "if"   ; forcedWS ; e1 <- expr ; forcedWS
            string "then" ; forcedWS ; e2 <- expr ; (forcedWS <?> "an 'else' branch")
            string "else" <?> "an 'else' branch" ; forcedWS ; If e1 e2 <$> expr

lambdaExpr = do char '\\' <|> char '\x03BB' <?> "anonymous function"
                whitespace
                args <- spaceSep1 patternTerm
                whitespace ; arrow ; whitespace
                e <- expr
                return $ makeFunction args e

assignExpr = do
  patterns <- spaceSep1 (patternTerm <?> "the definition of a variable (x = ...)")
  whitespace; string "="; whitespace; exp <- expr
  case patterns of
    PVar f : args -> return (f, makeFunction args exp)
    [PData x ps] -> if "Tuple" == take 5 x && all isDigit (drop 5 x) then
                        fail "matching tuples is not yet supported in this context"
                    else fail $ "Only tuples can be matched in this context, " ++
                                "not other abstract data types such as lists."
    _ -> fail $ "Variables in assign statement are not acceptable: " ++
                "only named variables, named functions, and tuples are okay."

letExpr = do
  string "let"
  brace <- optionMaybe . try $ do
             whitespace
             char '{' <?> "a set of definitions { x = ... ; y = ... }"
  case brace of
    Nothing -> do forcedWS; f <- assignExpr; forcedWS; string "in"; forcedWS;
                  Let [f] <$> expr
    Just '{' -> do whitespace ; fs <- semiSep1 assignExpr ; whitespace
                   string "}" <?> "closing bracket '}'"
                   whitespace; string "in"; forcedWS; e <- expr
                   return (Let fs e)

caseExpr = do
  string "case"; forcedWS; e <- expr; forcedWS; string "of"; whitespace
  Case e <$> brackets (semiSep1 (case_ <?> "cases { x -> ... }"))
    where case_ = do p <- patternExpr; whitespace; arrow; whitespace
                     (,) p <$> expr


expr = choice [ ifExpr, letExpr, caseExpr, lambdaExpr, binaryExpr ] <?> "expression"

def = do (f,e) <- assignExpr
         return ([f], [e], guid >>= \x -> return [VarT x])

defs1 = do optional freshLine
           d <- datatype <|> def
           (d:) <$> many (try (freshLine >> (datatype <|> def)))

defs = do
  (fss,ess,tss) <- unzip3 <$> defs1
  let (fs,es,ts) = (concat fss, concat ess, concat `liftM` sequence tss)
  optional freshLine ; optional whitespace ; eof
  return (Let (zip fs es) (Var "main"), liftM (zip fs) ts)

toDefs source = case parse defs "Elm code" source of
                  Right result -> Right result
                  Left err -> Left $ show err
