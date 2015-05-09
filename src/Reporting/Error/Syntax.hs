{-# OPTIONS_GHC -Wall #-}
module Reporting.Error.Syntax where

import qualified Data.List as List
import qualified Text.Parsec.Error as Parsec
import qualified Text.PrettyPrint as P
import Text.PrettyPrint ((<+>))

import qualified AST.Helpers as Help
import qualified AST.Type as Type
import Elm.Utils ((|>))
import qualified Reporting.PrettyPrint as P
import qualified Reporting.Report as Report


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


-- TO REPORT

toReport :: Error -> Report.Report
toReport err =
  case err of
    Parse messages ->
        Report.simple
          "SYNTAX PROBLEM"
          "Problem when parsing your code!"
          (unlines (map parseErrorHint messages))

    InfixDuplicate opName ->
        Report.simple
          "INFIX OVERLAP"
          ("The infix declarations for " ++ operator ++ " must be removed.")
          ("The precedence and associativity can only be set in one place, and\n"
           ++ "this information has already been set somewhere else."
          )
      where
        operator =
            if Help.isOp opName
              then "(" ++ opName ++ ")"
              else "`" ++ opName ++ "`"

    TypeWithoutDefinition valueName ->
        Report.simple
          "MISSING DEFINITION"
          ("There is a type annotation for '" ++ valueName ++ "' but there"
            ++ "is no corresponding definition!"
          )
          ("Directly below the type annotation, put a definition like:\n\n"
            ++ "    " ++ valueName ++ " = 42"
          )

    PortWithoutAnnotation portName ->
        Report.simple
          "PORT ERROR"
          ("Port '" ++ portName ++ "' does not have a type annotation!")
          ("Directly above the port definition, I need something like this:\n\n"
            ++ "    port " ++ portName ++ " : Signal Int"
          )

    DuplicateValueDeclaration name ->
        Report.simple
          "DUPLICATE DEFINITION"
          ("Naming multiple top-level values '" ++ name ++ "' makes things\n"
            ++ "ambiguous. When you say '" ++ name ++ "' which one do you want?!"
          )
          ("Find all the top-level values named '" ++ name ++ "' and\n"
            ++ "do some renaming. Make sure the names are distinct!"
          )

    DuplicateTypeDeclaration name ->
        Report.simple
          "DUPLICATE DEFINITION"
          ("Naming multiple types '" ++ name ++ "' makes things ambiguous\n"
            ++ "When you say '" ++ name ++ "' which one do you want?!"
          )
          ("Find all the types named '" ++ name ++ "' and\n"
            ++ "do some renaming. Make sure the names are distinct!"
          )

    DuplicateDefinition name ->
        Report.simple
          "DUPLICATE DEFINITION"
          ("Naming multiple values '" ++ name ++ "' in a single let-expression makes\n"
            ++ "things ambiguous. When you say '" ++ name ++ "' which one do you want?!"
          )
          ("Find all the values named '" ++ name ++ "' in this let-expression and\n"
            ++ "do some renaming. Make sure the names are distinct!"
          )

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


unboundTypeVars :: String -> String -> [String] -> String -> Report.Report
unboundTypeVars typeName tvar tvars revisedDeclaration =
  Report.simple
    "UNBOUND TYPE VARS"
    ( "Not all type variables in '" ++ typeName ++ "' are listed, making sneaky\n"
      ++ "type errors possible. Unbound type variables include: "
      ++ List.intercalate ", " (tvar:tvars)
    )
    ( "You probably want this definition instead:\n"
      ++ concatMap ("\n    "++) (lines revisedDeclaration) ++ "\n\n"
      ++ "Here's why. Imagine one '" ++ typeName ++ "' where '" ++ tvar ++ "' is an Int and\n"
      ++ "another where it is a Bool. When we explicitly list the type variables, type\n"
      ++ "checker can see that they are actually different types."
    )


parseErrorHint :: Parsec.Message -> String
parseErrorHint message =
  case message of
    Parsec.SysUnExpect msg ->
        "SysUnExpect: " ++ msg

    Parsec.UnExpect msg ->
        "UnExpect: " ++ msg

    Parsec.Expect msg ->
        "UnExpect: " ++ msg

    Parsec.Message msg ->
        "UnExpect: " ++ msg