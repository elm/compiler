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

chr :: IParser Literal
chr = Chr <$> betwixt '\'' '\'' (backslashed <|> satisfy (/='\''))
      <?> "character"

str :: IParser Literal
str = choice [ quote >> str <$> manyTill (backslashed <|> anyChar) quote
             , liftM Str . expecting "string" . betwixt '"' '"' . many $
               backslashed <|> satisfy (/='"')
             ]
    where
      quote = try (string "\"\"\"")
      str = Str . dewindows

      -- Remove \r from strings to fix generated JavaScript
      dewindows [] = []
      dewindows cs =
          let (pre, suf) = break (`elem` ['\r','\n']) cs
          in  pre ++ case suf of 
                       ('\r':'\n':rest) -> '\n' : dewindows rest
                       ('\n':rest)      -> '\n' : dewindows rest
                       ('\r':rest)      -> '\n' : dewindows rest
                       _                -> []
