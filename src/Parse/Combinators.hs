module Combinators where
import Control.Monad
import Data.Char
import Data.List (sortBy)

newtype Parser from to = Parser ([from] -> [(to,[from])])
--newtype Parser a = Parser (String -> [(a,String)])

parse (Parser p) = p

instance Monad (Parser from) where
    return a = Parser (\cs -> [(a,cs)])
    p >>= f  = Parser (\cs -> concat [parse (f a) cs' | (a,cs') <- parse p cs])

instance MonadPlus (Parser from) where
   mzero = Parser (\cs -> [])
   mplus p q = Parser (\cs -> parse p cs ++ parse q cs)

-- True recursive descent, tries everything.
p +++ q = mplus p q

-- Get the first parse.
p +|+ q = Parser (\cs -> case parse (mplus p q) cs of
                           [] -> []
                           (x:xs) -> [x])

zero = Parser (\cs -> [])

item  = Parser (\cs -> case cs of
                         [] -> []
                         (c:cs) -> [(c,cs)])

sat p = do {c <- item; if p c then return c else mzero}

char c = sat (c ==)

string ""     = return ""
string (c:cs) = do {char c; string cs; return (c:cs)}

star p = plus p +++ return []
plus p = do {a <- p; as <- star p; return (a:as)}

optional p = do { p >>= return . Just } +++ return Nothing

nospace = guard . (==0) . length =<< space
space = star $ sat isSpace
space1 = plus $ sat isSpace
newline = do
  star $ char ' ' +++ char '\t'
  char '\n'
  space
  return ""

digit = do {x <- sat isDigit; return (ord x - ord '0')}
integer = do {i <- plus digit; return $ foldl (\a d -> 10 * a + d) 0 i}

variable = do
  shd <- sat isLower;
  stl <- star $ sat (\c -> isAlphaNum c || c == '_')
  return $ shd:stl

sepBy sep p = sepBy1 sep p +++ return []

sepBy1 sep p = do
  x <- p
  xs <- star (sep >> p)
  return $ x:xs

select ps = foldl1 (+|+) ps

chainl p op a = (chainl1 p op) +++ return a
chainl1 p op = do {a <- p; rest a}
                where
                  rest a = (do f <- op
                               b <- p
                               rest (f a b)) +++ return a

extractResult err parse =
    case parse of
      (r,[]) : _ -> Right r
      _ : rest -> extractResult err rest
      [] -> Left err