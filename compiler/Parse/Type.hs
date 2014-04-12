{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
module Parse.Type where

import Control.Applicative ((<$>),(<*>),(<*))
import Data.List (intercalate)
import Text.Parsec

import qualified AST.Type as T
import qualified AST.Variable as Var
import Parse.Helpers

type RawType = T.Type Var.Raw

tvar :: IParser RawType
tvar = T.Var <$> lowVar <?> "type variable"

list :: IParser RawType
list = T.listOf <$> braces expr

tuple :: IParser RawType
tuple = do ts <- parens (commaSep expr)
           return $ case ts of
                      [t] -> t
                      _   -> T.tupleOf ts

record :: IParser RawType
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

constructor0 :: IParser RawType
constructor0 =
  do name <- capTypeVar
     return (T.Data (Var.Raw name) [])

term :: IParser RawType
term = list <|> tuple <|> record <|> tvar <|> constructor0

app :: IParser RawType
app =
  do name <- capTypeVar <|> try tupleCtor <?> "type constructor"
     args <- spacePrefix term
     return (T.Data (Var.Raw name) args)
  where
    tupleCtor = do
      n <- length <$> parens (many (char ','))
      return $ "_Tuple" ++ show (if n == 0 then 0 else n+1)

expr :: IParser RawType
expr =
  do t1 <- app <|> term
     arr <- optionMaybe $ try (whitespace >> arrow)
     case arr of
       Just _  -> T.Lambda t1 <$> (whitespace >> expr)
       Nothing -> return t1

constructor :: IParser (String, [RawType])
constructor = (,) <$> (capTypeVar <?> "another type constructor")
                  <*> spacePrefix term
