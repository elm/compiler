{-# OPTIONS_GHC -Wall #-}
module Reporting.Error.Syntax where

import qualified Data.Aeson as Json
import qualified Data.List as List
import qualified Text.Parsec.Error as Parsec
import qualified Text.PrettyPrint as P
import Text.PrettyPrint ((<+>))

import qualified AST.Helpers as Help
import qualified AST.Type as Type
import Elm.Utils ((|>))
import qualified Reporting.PrettyPrint as P


data Error
    = Parse [Parsec.Message]
    | InfixDuplicate String
    | TypeWithoutDefinition String
    | PortWithoutAnnotation String
    | DuplicateValueDeclaration String
    | DuplicateTypeDeclaration String
    | DuplicateDefinition String
    | UnboundTypeVarsInAlias String [String] String [String] Type.Raw
    | UnboundTypeVarsInUnion String [String] String [String] [(String, [Type.Raw])]


-- TO JSON

toJson :: Error -> Json.Value
toJson err =
  case err of
    _ -> error "Syntax.toJson"


-- HINTS

toHint :: Error -> String
toHint err =
  case err of
    Parse messages ->
        error "TODO" messages

    InfixDuplicate opName ->
        "The infix declarations for " ++ operator ++ " must be removed.\n\n"
        ++ "The precedence and associativity can only be set in one place, and\n"
        ++ "this information has already been set somewhere else."
      where
        operator =
            if Help.isOp opName
              then "(" ++ opName ++ ")"
              else "`" ++ opName ++ "`"

    TypeWithoutDefinition valueName ->
        "There is a type annotation for '" ++ valueName ++ "' but there"
        ++ "is no corresponding definition! I need something like this:\n\n"
        ++ "    " ++ valueName ++ " = 42\n\n"
        ++ "And be sure to put the definition directly below the type annotation."

    PortWithoutAnnotation portName ->
        "There is a port named '" ++ portName ++ "' but it does not have"
        ++ "a type annotation! I need something like this:\n\n"
        ++ indent ("port " ++ portName ++ " : Signal Int") ++ "\n\n"
        ++ "And be sure to put this annotation directly above the port definition."

    DuplicateValueDeclaration name ->
        "Multiple top-level declarations define '" ++ name ++ "',\n"
        ++ "but the name overlap makes all uses of '" ++ name ++ "' ambiguous.\n"
        ++ "How can we know which you want?!\n\n"
        ++ "Find all the top-level variables named '" ++ name ++ "' and\n"
        ++ "do some renaming. Make sure the names are distinct!"

    DuplicateTypeDeclaration tipeName ->
        "Multiple declarations define a type '" ++ tipeName ++ "',\n"
        ++ "but the name overlap makes all uses of '" ++ tipeName ++ "' ambiguous.\n"
        ++ "How can we know which you want?!\n\n"
        ++ "Find all the types named '" ++ tipeName ++ "' and\n"
        ++ "do some renaming. Make sure the names are distinct!"

    DuplicateDefinition name ->
        "This let-expression defines multiple variables named '" ++ name ++ "',\n"
        ++ "but the name overlap makes all uses of '" ++ name ++ "' ambiguous.\n"
        ++ "How can we know which you want?!\n\n"
        ++ "Find the variables named '" ++ name ++ "' in this let-expression and\n"
        ++ "do some renaming. Make sure the names are distinct!"

    UnboundTypeVarsInAlias typeName givenVars tvar tvars tipe ->
        unboundTypeVars typeName tvar tvars $ P.render $
            P.hang
              (P.text "type alias" <+> P.text typeName <+> P.hsep vars <+> P.equals)
              4
              (P.pretty False tipe)
      where
        vars = map P.text (givenVars ++ tvar : tvars)


    UnboundTypeVarsInUnion typeName givenVars tvar tvars ctors ->
        unboundTypeVars typeName tvar tvars $ P.render $
            P.vcat
              [ P.text "type" <+> P.text typeName <+> P.hsep vars
              , map toDoc ctors
                  |> zipWith (<+>) (P.text "=" : repeat (P.text "|"))
                  |> P.vcat
                  |> P.nest 4
              ]
      where
        vars = map P.text (givenVars ++ tvar : tvars)
        toDoc (ctor, args) =
            P.text ctor <+> P.hsep (map (P.pretty True) args)


unboundTypeVars :: String -> String -> [String] -> String -> String
unboundTypeVars typeName tvar tvars revisedDeclaration =
    "In the definition of type '" ++ typeName ++ "' you use the following\n"
    ++ "unbound type variable" ++ s ++ ": " ++ List.intercalate ", " (tvar:tvars) ++ "\n\n"
    ++ "All type variables must be listed to avoid sneaky type errors.\n"
    ++ "Imagine one '" ++ typeName ++ "' where '" ++ tvar ++ "' is an Int and\n"
    ++ "another where it is a Bool. They both look like a '" ++ typeName ++ "'\n"
    ++ "to the type checker, but they are actually different types!\n\n"
    ++ "Maybe you want a definition like this?\n\n"
    ++ indent revisedDeclaration ++ "\n\n"
  where
    s = if null tvars then "" else "s"


indent :: String -> String
indent string =
  List.intercalate "\n" (map ("  " ++ ) (lines string))
