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

listTerm = expecting "list" . betwixtSpcs '[' ']' $ choice
           [ try $ do { lo <- expr; symbol ".." ; Range lo <$> expr }
           , list <$> commaSep expr
           ]

parensTerm = expecting "parenthesized expression" . betwixtSpcs '(' ')' $ choice 
             [ do op <- anyOp
                  return . Lambda "x" . Lambda "y" $ Binop op (Var "x") (Var "y")
             , do es <- commaSep expr
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
  patterns <- (patternTerm <?> "definition") `endBy1` whitespace; symbol "="; exp <- expr
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

def = do (f,e) <- assignExpr
         return ([f], [e], guid >>= \x -> return [VarT x])

manyDefs = try (do { d <- anyDef ; ds <- many freshDef ; return (d:ds) })
           <|> try (many1 freshDef)
    where freshDef = try $ many1 freshLine >> anyDef
          anyDef = def <|> datatype

defs = do
  ds <- manyDefs
  let (fss,ess,tss) = unzip3 ds
  let (fs,es,ts) = (concat fss, concat ess, concat `liftM` sequence tss)
  optional freshLine ; optional whitespace ; eof
  return (Let (zip fs es) (Var "main"), liftM (zip fs) ts)

toDefs source = case parse defs "Elm code" source of
                  Right result -> Right result
                  Left err -> Left $ show err
