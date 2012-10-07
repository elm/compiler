module ParseExpr (def,term) where

import Ast
import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.Char (isSymbol, isDigit)
import Data.List (foldl')
import Text.Parsec hiding (newline,spaces)
import qualified Text.Pandoc as Pan

import ParseLib
import Patterns
import Binops

import Guid
import Types (Type (VarT), Scheme (Forall))

import System.IO.Unsafe


--------  Basic Terms  --------

numTerm :: (Monad m) => ParsecT [Char] u m Expr
numTerm = toExpr <$> (preNum <?> "number")
    where toExpr n | '.' `elem` n = FloatNum (read n)
                   | otherwise = IntNum (read n)
          preNum  = (++) <$> many1 digit <*> option "" postNum
          postNum = do try $ lookAhead (string "." >> digit)
                       string "."
                       ('.':) <$> many1 digit

strTerm :: (Monad m) => ParsecT [Char] u m Expr
strTerm = liftM Str . expecting "string" . betwixt '"' '"' . many $
          backslashed <|> satisfy (/='"')

varTerm :: (Monad m) => ParsecT [Char] u m Expr
varTerm = toVar <$> var <?> "variable"

toVar v = case v of "True"  -> Boolean True
                    "False" -> Boolean False
                    _       -> Var v

chrTerm :: (Monad m) => ParsecT [Char] u m Expr
chrTerm = Chr <$> betwixt '\'' '\'' (backslashed <|> satisfy (/='\''))
          <?> "character"


--------  Complex Terms  --------

listTerm = (do { try $ string "[markdown|"
               ; md <- filter (/='\r') <$> manyTill anyChar (try $ string "|]")
               ; return . Markdown $ Pan.readMarkdown Pan.defaultParserState md })
           <|> (braces $ choice
                [ try $ do { lo <- expr; whitespace; string ".." ; whitespace
                           ; Range lo <$> expr }
                , list <$> commaSep expr ])

parensTerm = parens $ choice
             [ do op <- anyOp
                  return . Lambda "x" . Lambda "y" $ Binop op (Var "x") (Var "y")
             , do let comma = char ',' <?> "comma ','"
                  commas <- comma >> many (whitespace >> comma)
                  let vars = map (('v':) . show) [ 0 .. length commas + 1 ]
                  return $ foldr Lambda (tuple $ map Var vars) (vars)
             , do es <- commaSep expr
                  return $ case es of { [e] -> e; _ -> tuple es }
             ]

term = choice [ numTerm, strTerm, chrTerm
              , accessible varTerm
              , listTerm, parensTerm ]
       <?> "basic term (4, x, 'c', etc.)"

--------  Applications  --------

appExpr = do
  tlist <- spaceSep1 term
  return $ case tlist of
             t:[] -> t
             t:ts -> foldl' App t ts

--------  Normal Expressions  --------

binaryExpr = binops appExpr anyOp

ifExpr = do reserved "if"   ; whitespace ; e1 <- expr ; whitespace
            reserved "then" ; whitespace ; e2 <- expr ; (whitespace <?> "an 'else' branch")
            reserved "else" <?> "an 'else' branch" ; whitespace ; If e1 e2 <$> expr

lambdaExpr = do char '\\' <|> char '\x03BB' <?> "anonymous function"
                whitespace
                pats <- spaceSep1 patternTerm
                whitespace ; arrow ; whitespace
                e <- expr
                return $ makeLambda pats e

letExpr = do
  reserved "let"
  brace <- optionMaybe . try $ do
             whitespace
             char '{' <?> "a set of definitions { x = ... ; y = ... }"
  case brace of
    Nothing -> do whitespace; ds <- assignExpr
                  whitespace; reserved "in"; whitespace; Let ds <$> expr
    Just '{' -> do whitespace ; dss <- semiSep1 assignExpr ; whitespace
                   string "}" <?> "closing bracket '}'"
                   whitespace; reserved "in"; whitespace; e <- expr
                   return $ Let (concat dss) e

caseExpr = do
  reserved "case"; whitespace; e <- expr; whitespace; reserved "of"; whitespace
  Case e <$> brackets (semiSep1 (case_ <?> "cases { x -> ... }"))
    where case_ = do p <- patternExpr; whitespace; arrow; whitespace
                     (,) p <$> expr

expr = choice [ ifExpr, letExpr, caseExpr
              , lambdaExpr, binaryExpr ] <?> "an expression"

assignExpr = do
  patterns <-
      choice [ try $ do v <- PVar <$> lowVar
                        notFollowedBy (whitespace >> char ':')
                        (v:) <$> spacePrefix patternTerm
             , (:[]) <$> patternExpr
             ] <?> "the definition of a variable (x = ...)"
  whitespace; string "="; whitespace; exp <- expr
  flattenPatterns patterns exp

def = map (\(Definition n as e) -> Def n as e) <$> assignExpr


parseDef str =
    case parse def "" str of
      Right result -> Right result
      Left err -> Left $ "Parse error at " ++ show err
