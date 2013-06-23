module Parse.Literal (literal) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Text.Parsec hiding (newline,spaces)
import Text.Parsec.Indent

import Parse.Helpers
import SourceSyntax.Literal

literal = num <|> str <|> chr

num :: IParser Literal
num = fmap toLit (preNum <?> "number")
    where toLit n | '.' `elem` n = FloatNum (read n)
                  | otherwise = IntNum (read n)
          preNum  = (++) <$> many1 digit <*> option "" postNum
          postNum = do try $ lookAhead (string "." >> digit)
                       string "."
                       ('.':) <$> many1 digit

str :: IParser Literal
str = liftM Str . expecting "string" . betwixt '"' '"' . many $
          backslashed <|> satisfy (/='"')

chr :: IParser Literal
chr = Chr <$> betwixt '\'' '\'' (backslashed <|> satisfy (/='\''))
      <?> "character"