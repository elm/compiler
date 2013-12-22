module Parse.Literal (literal) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Text.Parsec hiding (newline,spaces)
import Text.Parsec.Indent

import Parse.Helpers
import SourceSyntax.Literal

literal = num <|> (Str <$> str) <|> (Chr <$> chr)

num :: IParser Literal
num = fmap toLit (preNum <?> "number")
    where toLit n | '.' `elem` n = FloatNum (read n)
                  | otherwise = IntNum (read n)
          preNum  = concat <$> sequence [ option "" minus, many1 digit, option "" postNum ]
          postNum = do try $ lookAhead (string "." >> digit)
                       string "."
                       ('.':) <$> many1 digit
          minus = try $ do string "-"
                           lookAhead digit
                           return "-"