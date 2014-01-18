{-# OPTIONS_GHC -Wall #-}
module SourceSyntax.Pattern where

import qualified SourceSyntax.Helpers as Help
import SourceSyntax.PrettyPrint
import Text.PrettyPrint as PP
import qualified Data.Set as Set
import SourceSyntax.Literal as Literal

data Pattern = PData String [Pattern]
             | PRecord [String]
             | PAlias String Pattern
             | PVar String
             | PAnything
             | PLiteral Literal.Literal
               deriving (Eq, Ord, Show)

cons :: Pattern -> Pattern -> Pattern
cons h t = PData "::" [h,t]

nil :: Pattern
nil = PData "[]" []

list :: [Pattern] -> Pattern
list     = foldr cons nil

tuple :: [Pattern] -> Pattern
tuple es = PData ("_Tuple" ++ show (length es)) es

boundVars :: Pattern -> Set.Set String
boundVars pattern =
    case pattern of
      PVar x -> Set.singleton x
      PAlias x p -> Set.insert x (boundVars p)
      PData _ ps -> Set.unions (map boundVars ps)
      PRecord fields -> Set.fromList fields
      PAnything -> Set.empty
      PLiteral _ -> Set.empty


instance Pretty Pattern where
  pretty pattern =
   case pattern of
     PVar x -> variable x
     PLiteral lit -> pretty lit
     PRecord fs -> PP.braces (commaCat $ map variable fs)
     PAlias x p -> prettyParens p <+> PP.text "as" <+> variable x
     PAnything -> PP.text "_"
     PData "::" [hd,tl] -> parensIf isCons (pretty hd) <+> PP.text "::" <+> pretty tl
       where isCons = case hd of
                        PData "::" _ -> True
                        _ -> False
     PData name ps ->
        if Help.isTuple name then
            PP.parens . commaCat $ map pretty ps
        else hsep (PP.text name : map prettyParens ps)

prettyParens :: Pattern -> Doc
prettyParens pattern = parensIf needsThem (pretty pattern)
  where
    needsThem =
      case pattern of
        PData name (_:_) | not (Help.isTuple name) -> True
        PAlias _ _ -> True
        _ -> False
