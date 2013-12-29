module SourceSyntax.Pattern where

import Data.List (intercalate)
import SourceSyntax.Helpers as Help
import SourceSyntax.PrettyPrint
import Text.PrettyPrint as PP
import SourceSyntax.Literal as Literal

data Pattern = PData String [Pattern]
             | PRecord [String]
             | PAlias String Pattern
             | PVar String
             | PAnything
             | PLiteral Literal.Literal
               deriving (Eq, Ord, Show)

cons h t = PData "::" [h,t]
nil      = PData "[]" []
list     = foldr cons nil
tuple es = PData ("_Tuple" ++ show (length es)) es


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
        if isTuple name then
            PP.parens . commaCat $ map pretty ps
        else sep (PP.text name : map prettyParens ps)

prettyParens pattern = parensIf needsThem (pretty pattern)
  where
    needsThem =
      case pattern of
        PData name (_:_) | not (isTuple name) -> True
        PAlias _ _ -> True
        _ -> False