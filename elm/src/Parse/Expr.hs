module Parse.Expr (def,term) where

import Ast
import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.Char (isSymbol, isDigit)
import Data.List (foldl')
import Text.Parsec hiding (newline,spaces)
import Text.Parsec.Indent
import qualified Text.Pandoc as Pan

import Parse.Library
import Parse.Patterns
import Parse.Binops

import Guid
import Types.Types (Type (VarT), Scheme (Forall))

import System.IO.Unsafe


--------  Basic Terms  --------

numTerm :: IParser Expr
numTerm = toExpr <$> (preNum <?> "number")
    where toExpr n | '.' `elem` n = FloatNum (read n)
                   | otherwise = IntNum (read n)
          preNum  = (++) <$> many1 digit <*> option "" postNum
          postNum = do try $ lookAhead (string "." >> digit)
                       string "."
                       ('.':) <$> many1 digit

strTerm :: IParser Expr
strTerm = liftM Str . expecting "string" . betwixt '"' '"' . many $
          backslashed <|> satisfy (/='"')

varTerm :: IParser Expr
varTerm = toVar <$> var <?> "variable"

toVar v = case v of "True"  -> Boolean True
                    "False" -> Boolean False
                    _       -> Var v

chrTerm :: IParser Expr
chrTerm = Chr <$> betwixt '\'' '\'' (backslashed <|> satisfy (/='\''))
          <?> "character"


--------  Complex Terms  --------

listTerm :: IParser Expr
listTerm = (do { try $ string "[markdown|"
               ; md <- filter (/='\r') <$> manyTill anyChar (try $ string "|]")
               ; return . Markdown $ Pan.readMarkdown Pan.defaultParserState md })
           <|> (braces $ choice
                [ try $ do { lo <- expr; whitespace; string ".." ; whitespace
                           ; Range lo <$> expr }
                , list <$> commaSep expr ])

parensTerm :: IParser Expr
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

term :: IParser Expr
term = choice [ numTerm, strTerm, chrTerm
              , accessible varTerm
              , listTerm, parensTerm ]
       <?> "basic term (4, x, 'c', etc.)"

--------  Applications  --------

appExpr :: IParser Expr
appExpr = do
  tlist <- spaceSep1 term
  return $ case tlist of
             t:[] -> t
             t:ts -> foldl' App t ts

--------  Normal Expressions  --------

binaryExpr :: IParser Expr
binaryExpr = binops appExpr anyOp

ifExpr :: IParser Expr
ifExpr = do reserved "if"   ; whitespace ; e1 <- expr ; whitespace
            reserved "then" ; whitespace ; e2 <- expr ; (whitespace <?> "an 'else' branch")
            reserved "else" <?> "an 'else' branch" ; whitespace ; If e1 e2 <$> expr

lambdaExpr :: IParser Expr
lambdaExpr = do char '\\' <|> char '\x03BB' <?> "anonymous function"
                whitespace
                pats <- spaceSep1 patternTerm
                whitespace ; arrow ; whitespace
                e <- expr
                return $ makeLambda pats e

defSet :: IParser [Def]
defSet = do
  brace <- optionMaybe $ do
             string "{" <?> "a set of definitions { x = ... ; y = ... }"
             whitespace >> return "{"
  case brace of
    Nothing  -> concat <$> block (do d <- assignExpr ; whitespace ; return d)
    Just "{" -> do dss <- semiSep1 assignExpr
                   whitespace
                   string "}" <?> "closing bracket '}'"
                   return (concat dss)

letExpr :: IParser Expr
letExpr = do
  reserved "let" ; whitespace
  defs <- defSet
  whitespace ; reserved "in" ; whitespace
  Let defs <$> expr

caseExpr :: IParser Expr
caseExpr = do
  reserved "case"; whitespace; e <- expr; whitespace; reserved "of"; whitespace
  Case e <$> (with <|> without)
    where case_ = do p <- patternExpr; whitespace; arrow; whitespace
                     (,) p <$> expr
          with    = brackets (semiSep1 (case_ <?> "cases { x -> ... }"))
          without = block (do c <- case_ ; whitespace ; return c)

expr = choice [ ifExpr, letExpr, caseExpr
              , lambdaExpr, binaryExpr ] <?> "an expression"

funcDef = try (do p1 <- try patternTerm ; infics p1 <|> func p1)
          <|> ((:[]) <$> patternExpr)
          <?> "the definition of a variable (x = ...)"
    where func p@(PVar v) = (p:) <$> spacePrefix patternTerm
          func p          = do try (lookAhead (whitespace >> string "="))
                               return [p]
          infics p1 = do
            o:p <- try (whitespace >> anyOp)
            p2  <- (whitespace >> patternTerm)
            return $ if o == '`' then [ PVar $ takeWhile (/='`') p, p1, p2 ]
                                 else [ PVar (o:p), p1, p2 ]

assignExpr :: IParser [Def]
assignExpr = withPos $ do
  fDefs <- funcDef
  whitespace
  e <- (string "=" >> whitespace >> expr) <|> guardExpr
  flattenPatterns fDefs e

guardExpr = (Guard <$> spaceSep1 gExpr)
    where gExpr = do string "|" ; whitespace
                     b <- expr ; whitespace ; string "=" ; whitespace
                     (,) b <$> expr

def = map Definition <$> assignExpr


parseDef str =
    case iParse def "" str of
      Right result -> Right result
      Left err -> Left $ "Parse error at " ++ show err
