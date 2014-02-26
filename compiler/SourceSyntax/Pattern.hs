{-# OPTIONS_GHC -Wall #-}
module SourceSyntax.Pattern where

import qualified SourceSyntax.Helpers as Help
import SourceSyntax.PrettyPrint
import Text.PrettyPrint as PP
import qualified Data.Set as Set
import SourceSyntax.Literal as Literal

data Pattern
    = Data String [Pattern]
    | Record [String]
    | Alias String Pattern
    | Var String
    | Anything
    | Literal Literal.Literal
    deriving (Eq, Ord, Show)

cons :: Pattern -> Pattern -> Pattern
cons h t = Data "::" [h,t]

nil :: Pattern
nil = Data "[]" []

list :: [Pattern] -> Pattern
list     = foldr cons nil

tuple :: [Pattern] -> Pattern
tuple es = Data ("_Tuple" ++ show (length es)) es

boundVarList :: Pattern -> [String]
boundVarList = Set.toList . boundVars

boundVars :: Pattern -> Set.Set String
boundVars pattern =
    case pattern of
      Var x -> Set.singleton x
      Alias x p -> Set.insert x (boundVars p)
      Data _ ps -> Set.unions (map boundVars ps)
      Record fields -> Set.fromList fields
      Anything -> Set.empty
      Literal _ -> Set.empty


instance Pretty Pattern where
  pretty pattern =
   case pattern of
     Var x -> variable x
     Literal lit -> pretty lit
     Record fs -> PP.braces (commaCat $ map variable fs)
     Alias x p -> prettyParens p <+> PP.text "as" <+> variable x
     Anything -> PP.text "_"
     Data "::" [hd,tl] -> parensIf isCons (pretty hd) <+> PP.text "::" <+> pretty tl
       where isCons = case hd of
                        Data "::" _ -> True
                        _ -> False
     Data name ps ->
        if Help.isTuple name then
            PP.parens . commaCat $ map pretty ps
        else hsep (PP.text name : map prettyParens ps)

prettyParens :: Pattern -> Doc
prettyParens pattern = parensIf needsThem (pretty pattern)
  where
    needsThem =
      case pattern of
        Data name (_:_) | not (Help.isTuple name) -> True
        Alias _ _ -> True
        _ -> False
