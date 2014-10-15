{-# OPTIONS_GHC -Wall #-}
module AST.Pattern where

import qualified AST.Helpers as Help
import AST.PrettyPrint
import Text.PrettyPrint as PP
import qualified Data.Set as Set

import qualified AST.Variable as Var
import AST.Literal as Literal

data Pattern var
    = Data var [Pattern var]
    | Record [String]
    | Alias String (Pattern var)
    | Var String
    | Anything
    | Literal Literal.Literal
    deriving (Eq, Ord, Show)

type RawPattern = Pattern Var.Raw
type CanonicalPattern = Pattern Var.Canonical

cons :: RawPattern -> RawPattern -> RawPattern
cons h t = Data (Var.Raw "::") [h,t]

nil :: RawPattern
nil = Data (Var.Raw "[]") []

list :: [RawPattern] -> RawPattern
list = foldr cons nil

tuple :: [RawPattern] -> RawPattern
tuple es = Data (Var.Raw ("_Tuple" ++ show (length es))) es

boundVarList :: Pattern var -> [String]
boundVarList = Set.toList . boundVars

boundVars :: Pattern var -> Set.Set String
boundVars pattern =
    case pattern of
      Var x -> Set.singleton x
      Alias x p -> Set.insert x (boundVars p)
      Data _ ps -> Set.unions (map boundVars ps)
      Record fields -> Set.fromList fields
      Anything -> Set.empty
      Literal _ -> Set.empty


instance Var.ToString var => Pretty (Pattern var) where
  pretty pattern =
   case pattern of
     Var x -> variable x
     Literal lit -> pretty lit
     Record fs -> PP.braces (commaCat $ map variable fs)
     Alias x p -> prettyParens p <+> PP.text "as" <+> variable x
     Anything -> PP.text "_"
     Data name [hd,tl] | Var.toString name == "::" ->
         parensIf isCons (pretty hd) <+> PP.text "::" <+> pretty tl
       where
         isCons = case hd of
                    Data ctor _ -> Var.toString ctor == "::"
                    _ -> False

     Data name ps
         | Help.isTuple name' -> PP.parens . commaCat $ map pretty ps
         | otherwise          -> hsep (PP.text name' : map prettyParens ps)
         where
           name' = Var.toString name

prettyParens :: Var.ToString var => Pattern var -> Doc
prettyParens pattern = 
    parensIf needsThem (pretty pattern)
  where
    needsThem =
      case pattern of
        Data name (_:_) | not (Help.isTuple (Var.toString name)) -> True
        Alias _ _ -> True
        _ -> False
