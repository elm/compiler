{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
module Parse.Type where

import Control.Applicative ((<$>),(<*>),(<*))
import Data.List (intercalate)
import Text.Parsec

import SourceSyntax.Type as T
import Parse.Helpers

tvar :: IParser T.Type
tvar = T.Var <$> lowVar <?> "type variable"

list :: IParser T.Type
list = listOf <$> braces expr

tuple :: IParser T.Type
tuple = do ts <- parens (commaSep expr)
           return $ case ts of
                      [t] -> t
                      _   -> tupleOf ts

record :: IParser T.Type
record =
  do char '{' ; whitespace
     rcrd <- extended <|> normal
     dumbWhitespace ; char '}'
     return rcrd
  where
    normal = flip T.Record Nothing <$> commaSep field

    -- extended record types require at least one field
    extended = do
      ext <- try (lowVar <* (whitespace >> string "|"))
      whitespace
      flip T.Record (Just ext) <$> commaSep1 field

    field = do
      lbl <- rLabel
      whitespace >> hasType >> whitespace
      (,) lbl <$> expr

capTypeVar :: IParser String
capTypeVar = intercalate "." <$> dotSep1 capVar

constructor0 :: IParser T.Type
constructor0 =
  do name <- capTypeVar
     return (T.Data name [])

term :: IParser T.Type
term = list <|> tuple <|> record <|> tvar <|> constructor0

app :: IParser T.Type
app =
  do name <- capTypeVar <|> try tupleCtor <?> "type constructor"
     args <- spacePrefix term
     return (T.Data name args)
  where
    tupleCtor = do
      n <- length <$> parens (many (char ','))
      return $ "_Tuple" ++ show (if n == 0 then 0 else n+1)

expr :: IParser T.Type
expr =
  do t1 <- app <|> term
     arr <- optionMaybe $ try (whitespace >> arrow)
     case arr of
       Just _  -> T.Lambda t1 <$> (whitespace >> expr)
       Nothing -> return t1

constructor :: IParser (String, [T.Type])
constructor = (,) <$> (capTypeVar <?> "another type constructor")
                  <*> spacePrefix term
