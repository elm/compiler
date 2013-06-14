{-# LANGUAGE DeriveDataTypeable #-}
module SourceSyntax.Pattern where

import Data.Char (isDigit)
import Data.List (intercalate)
import Data.Data
import SourceSyntax.Helpers
import qualified SourceSyntax.Literal as Literal

data Pattern = PData String [Pattern]
             | PRecord [String]
             | PAlias String Pattern
             | PVar String
             | PAnything
             | PLiteral Literal.Literal
               deriving (Eq, Data, Typeable)

cons h t = PData "Cons" [h,t]
nil      = PData "Nil" []
list     = foldr cons nil
tuple es = PData ("Tuple" ++ show (length es)) es


instance Show Pattern where
  show p =
   case p of
     PVar x -> x
     PLiteral lit -> show lit
     PRecord fs -> brkt (intercalate ", " fs)
     PAlias x p -> show p ++ " as " ++ x
     PAnything -> "_"
     PData "Cons" [hd@(PData "Cons" _),tl] ->
        parens (show hd) ++ " :: " ++ show tl
     PData "Cons" [hd,tl] -> show hd ++ " :: " ++ show tl
     PData "Nil" [] -> "[]"
     PData name ps ->
        if take 5 name == "Tuple" && all isDigit (drop 5 name) then
            parens . intercalate ", " $ map show ps
        else parensIf (not (null ps)) $ unwords (name : map show ps)