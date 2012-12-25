
module Parse.Library where

import Ast
import Context
import Control.Applicative ((<$>),(<*>))
import Control.Monad
import Control.Monad.State
import Data.Char (isSymbol)
import Text.Parsec hiding (newline,spaces,State)
import Text.Parsec.Indent

reserveds = [ "if", "then", "else"
            , "case", "of", "data"
            , "let", "in"
            , "module", "where"
            , "import", "as", "hiding"
            , "export", "foreign" ]

expecting = flip (<?>)

type IParser a = ParsecT String () (State SourcePos) a

iParse :: IParser a -> SourceName -> String -> Either ParseError a
iParse aParser source_name input =
  runIndent source_name $ runParserT aParser () source_name input

backslashed :: IParser Char
backslashed = do { char '\\'; c <- anyChar
                 ; return . read $ ['\'','\\',c,'\''] }

var :: IParser String
var = makeVar (letter <|> char '_' <?> "variable")

lowVar :: IParser String
lowVar = makeVar (lower <?> "lower case variable")
capVar :: IParser String
capVar = makeVar (upper <?> "upper case variable")

innerVarChar :: IParser Char
innerVarChar = alphaNum <|> char '_' <|> char '\'' <?> "" 

makeVar :: IParser Char -> IParser String
makeVar p = do v <- (:) <$> p <*> many innerVarChar
               guard (v `notElem` reserveds)
               return v

reserved :: String -> IParser String
reserved word =
  try (string word >> notFollowedBy innerVarChar) >> return word
  <?> "reserved word '" ++ word ++ "'"

anyOp :: IParser String
anyOp = betwixt '`' '`' var <|> symOp <?> "infix operator (e.g. +, *, ||)"

isOp c = isSymbol c || elem c "+-/*=.$<>:&|^?%#@~!"

symOp :: IParser String
symOp = do op <- many1 (satisfy isOp)
           guard (op `notElem` [ "=", "..", "->", "--", "|", "\8594" ])
           case op of
             "." -> notFollowedBy lower >> return op
             _   -> return op

arrow :: IParser String
arrow = string "->" <|> string "\8594" <?> "arrow (->)"


commitIf check p = commit <|> try p
    where commit = do (try $ lookAhead check) >> p

spaceySepBy1 :: IParser b -> IParser a -> IParser [a]
spaceySepBy1 sep p = do
  a <- p
  (a:) <$> many (commitIf (whitespace >> sep) (whitespace >> sep >> whitespace >> p))


commaSep1 :: IParser a -> IParser [a]
commaSep1 = spaceySepBy1 (char ',' <?> "comma ','")

commaSep :: IParser a -> IParser [a]
commaSep = option [] . commaSep1

semiSep1 :: IParser a -> IParser [a]
semiSep1 = spaceySepBy1 (char ';' <?> "semicolon ';'")

pipeSep1 :: IParser a -> IParser [a]
pipeSep1 = spaceySepBy1 (char '|' <?> "type divider '|'")

consSep1 :: IParser a -> IParser [a]
consSep1 = spaceySepBy1 (char ':' <?> "cons operator ':'")

dotSep1 :: IParser a -> IParser [a]
dotSep1 p = (:) <$> p <*> many (try (char '.') >> p)

spaceSep1 :: IParser a -> IParser [a]
spaceSep1 p =  (:) <$> p <*> spacePrefix p

spacePrefix p = many (try (whitespace >> indented >> p))

followedBy a b = do x <- a ; b ; return x

betwixt a b c = do char a ; out <- c
                   char b <?> "closing '" ++ [b] ++ "'" ; return out

surround a z name p = do
  char a ; whitespace ; v <- p ; whitespace
  char z <?> unwords ["closing", name, show z]
  return v

braces   :: IParser a -> IParser a
braces   = surround '[' ']' "brace"

parens   :: IParser a -> IParser a
parens   = surround '(' ')' "paren"

brackets :: IParser a -> IParser a
brackets = surround '{' '}' "bracket"

addContext :: IParser Expr -> IParser CExpr
addContext expr = do
  start <- getPosition
  e <- expr
  end <- getPosition
  return (pos start end e)

accessible :: IParser CExpr -> IParser CExpr
accessible expr = do
  start <- getPosition
  e <- expr
  access <- optionMaybe . try $ (do { char '.' ; notFollowedBy (char '.') }
                                    <?> "field access (e.g. List.map)")
  case access of
    Just _  -> accessible $ do v <- var <?> "field access (e.g. List.map)"
                               end <- getPosition
                               return (pos start end (Access e v))
    Nothing -> return e


spaces :: IParser String
spaces = many1 ((multiComment <|> string " " <?> "") >> return ' ') <?> "spaces"

forcedWS :: IParser [String]
forcedWS = try (do { spaces; many nl_space }) <|> try (many1 nl_space)
    where nl_space = try $ many1 newline >> spaces

whitespace :: IParser ()
whitespace = optional forcedWS <?> ""

freshLine :: IParser [[String]]
freshLine = try (do { many1 newline; many space_nl }) <|> try (many1 space_nl) <?> ""
    where space_nl = try $ spaces >> many1 newline

newline :: IParser String
newline = simpleNewline <|> lineComment <?> ""

simpleNewline :: IParser String
simpleNewline = try (string "\r\n") <|> string "\n"

lineComment :: IParser String
lineComment = do try (string "--")
                 manyTill anyChar $ simpleNewline <|> (eof >> return "\n")

multiComment :: IParser String
multiComment = do { try (string "{-"); closeComment }

closeComment :: IParser String
closeComment = manyTill anyChar . choice $
               [ try (string "-}") <?> "close comment"
               , do { try $ string "{-"; closeComment; closeComment }
               ]