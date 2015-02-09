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


type RawPattern =
    Pattern Var.Raw


type CanonicalPattern =
    Pattern Var.Canonical


cons :: RawPattern -> RawPattern -> RawPattern
cons h t =
  Data (Var.Raw "::") [h,t]


nil :: RawPattern
nil =
  Data (Var.Raw "[]") []


list :: [RawPattern] -> RawPattern
list patterns =
  foldr cons nil patterns


tuple :: [RawPattern] -> RawPattern
tuple patterns =
  Data (Var.Raw ("_Tuple" ++ show (length patterns))) patterns


boundVarList :: Pattern var -> [String]
boundVarList pattern =
  Set.toList (boundVars pattern)


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
      Var x ->
          variable x

      Literal literal ->
          pretty literal

      Record fields ->
          PP.braces (commaCat (map variable fields))

      Alias x p ->
          prettyParens p <+> PP.text "as" <+> variable x

      Anything ->
          PP.text "_"

      Data name [hd,tl] | Var.toString name == "::" ->
          parensIf isCons (pretty hd) <+> PP.text "::" <+> pretty tl
        where
          isCons =
            case hd of
              Data ctor _ -> Var.toString ctor == "::"
              _ -> False
 
      Data name ps ->
        let name' = Var.toString name
        in
            if Help.isTuple name'
              then PP.parens (commaCat (map pretty ps))
              else hsep (PP.text name' : map prettyParens ps)
          

prettyParens :: Var.ToString var => Pattern var -> Doc
prettyParens pattern = 
    parensIf needsThem (pretty pattern)
  where
    needsThem =
      case pattern of
        Data name (_:_) | not (Help.isTuple (Var.toString name)) -> True
        Alias _ _ -> True
        _ -> False
