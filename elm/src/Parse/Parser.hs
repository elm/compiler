module Parser where

import Ast
import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.Char (isSymbol, isDigit)
import Data.List (foldl')
import Text.Parsec

import ParseLib
import Patterns
import Binop

--------  Basic Terms  --------

numTerm :: (Monad m) => ParsecT [Char] u m Expr
numTerm = liftM (Number . read) (many1 digit) <?> "number"

strTerm :: (Monad m) => ParsecT [Char] u m Expr
strTerm = expecting "string" . liftM Str . betwixt '"' '"' . many $ noneOf "\""

varTerm :: (Monad m) => ParsecT [Char] u m Expr
varTerm = toVar <$> var <?> "variable"

toVar v = case v of "True"  -> Boolean True
                    "False" -> Boolean False
                    _       -> Var v

chrTerm :: (Monad m) => ParsecT [Char] u m Expr
chrTerm = Chr <$> betwixt '\'' '\'' anyChar <?> "character"


--------  Complex Terms  --------

listTerm = expecting "list" . betwixtSpcs '[' ']' $ choice
           [ try $ do { lo <- expr; string ".." ; Range lo <$> expr }
           , list <$> expr `sepBy` lexeme (char ',')
           ]

parensTerm = expecting "parenthesized expression" . betwixtSpcs '(' ')' $ choice 
             [ do op <- anyOp
                  return . Lambda "x" . Lambda "y" $ Binop op (Var "x") (Var "y")
             , do es <- expr `sepBy` lexeme (char ',')
                  return $ case es of { [e] -> e; _ -> tuple es }
             ]

term = lexeme (choice [ numTerm, strTerm, chrTerm
                      , accessible varTerm
                      , listTerm, parensTerm ])
       <?> "basic term (number, variable, etc.)"

--------  Applications  --------

appExpr = do
  tlist <- many1 (try term)
  return $ case tlist of
             t:[] -> t
             t:ts -> foldl' App t ts

--------  Normal Expressions  --------

binaryExpr = binops appExpr anyOp

ifExpr = expecting "if expression" $
         do symbol "if"   ; e1 <- expr
            symbol "then" ; e2 <- expr
            expecting "else branch" $ do
              symbol "else" ; If e1 e2 <$> expr

lambdaExpr = do lexeme $ oneOf "\\\x03BB"
                args <- patternTerm `endBy1` whitespace
                arrow
                e <- expr
                return $ makeFunction args e

assignExpr = do
  patterns <- patternTerm `endBy1` whitespace; symbol "="; exp <- expr
  case patterns of
    PVar f : args -> return (f, makeFunction args exp)
    [PData x ps] -> if "Tuple" == take 5 x && all isDigit (drop 5 x) then
                        fail "matching tuples is not yet supported in this context"
                    else fail $ "Only tuples can be matched in this context, " ++
                                "not other abstract data types such as lists."
    _ -> fail $ "Variables in assign statement are not acceptable: " ++
                "only named variables, named functions, and tuples are okay."

letExpr = do
  symbol "let"; brace <- optionMaybe . lexeme $ char '{'
  case brace of
    Nothing -> do f <- assignExpr; symbol "in"; e <- expr; return (Let [f] e)
    Just '{' -> do fs <- assignExpr `sepBy1` symbol ";"
                   symbol "}" ; symbol "in"; e <- expr
                   return (Let fs e)

caseExpr = do
  symbol "case"; e <- expr; symbol "of"
  betwixtSpcs '{' '}' $ Case e <$> case_ `sepBy1` symbol ";"
    where case_ = do p <- patternExpr; whitespace; arrow; e <- expr; return (p,e)


expr = choice [ ifExpr, letExpr, caseExpr, lambdaExpr, binaryExpr ]

aoeu = parse (between whitespace eof expr) ""

symbol = lexeme . try . string 
arrow  = symbol "->" <|> symbol "\8594"

def = do (f,e) <- assignExpr
         return ([f], [e], guid >>= \x -> return [VarT x])

{--
defs = do
  (fss,ess,tss) <- unzip3 `liftM` plus (whitespace >> plus (sat (==NEWLINE)) >> def +|+ datatype)
  let (fs,es,ts) = (concat fss, concat ess, concat `liftM` sequence tss)
  star $ sat (==NEWLINE) +|+ sat (==SPACES)
  return (Let (zip fs es) (Var "main"), liftM (zip fs) ts)

err = "Parse Error: Better error messages to come!"
toDefs = parse defs . ('\n':)
--}