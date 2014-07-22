{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
module Parse.Type where

import Control.Applicative ((<$>),(<*>),(<*))
import Data.List (intercalate)
import Text.Parsec

import qualified AST.Type as T
import qualified AST.Variable as Var
import Parse.Helpers

tvar :: IParser T.RawType
tvar = T.Var <$> lowVar <?> "type variable"

list :: IParser T.RawType
list = T.listOf <$> braces expr

tuple :: IParser T.RawType
tuple = do ts <- parens (commaSep expr)
           return $ case ts of
                      [t] -> t
                      _   -> T.tupleOf ts

record :: IParser T.RawType
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
      flip T.Record (Just (T.Var ext)) <$> commaSep1 field

    field = do
      lbl <- rLabel
      whitespace >> hasType >> whitespace
      (,) lbl <$> expr

capTypeVar :: IParser String
capTypeVar = intercalate "." <$> dotSep1 capVar

constructor0 :: IParser T.RawType
constructor0 =
  do name <- capTypeVar
     return (T.Type (Var.Raw name))

term :: IParser T.RawType
term = list <|> tuple <|> record <|> tvar <|> constructor0

app :: IParser T.RawType
app =
  do f <- constructor0 <|> try tupleCtor <?> "type constructor"
     args <- spacePrefix term
     return $ case args of
                [] -> f
                _  -> T.App f args
  where
    tupleCtor = do
      n <- length <$> parens (many (char ','))
      let ctor = "_Tuple" ++ show (if n == 0 then 0 else n+1)
      return (T.Type (Var.Raw ctor))

expr :: IParser T.RawType
expr =
  do t1 <- app <|> term
     arr <- optionMaybe $ try (whitespace >> arrow)
     case arr of
       Just _  -> T.Lambda t1 <$> (whitespace >> expr)
       Nothing -> return t1

constructor :: IParser (String, [T.RawType])
constructor = (,) <$> (capTypeVar <?> "another type constructor")
                  <*> spacePrefix term
