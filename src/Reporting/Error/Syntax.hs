{-# OPTIONS_GHC -Wall #-}
module Reporting.Error.Syntax where

import qualified Data.List as List
import qualified Data.Set as Set
import qualified Text.Parsec.Error as Parsec

import qualified AST.Helpers as Help
import qualified AST.Type as Type
import qualified Reporting.Render.Type as RenderType
import qualified Reporting.Report as Report


data Error
    = Parse [Parsec.Message]
    | BadFunctionName Int
    | BadPattern String
    | InfixDuplicate String
    | TypeWithoutDefinition String
    | PortWithoutAnnotation String
    | UnexpectedPort
    | DuplicateFieldName String
    | DuplicateValueDeclaration String
    | DuplicateTypeDeclaration String
    | DuplicateDefinition String
    | UnboundTypeVarsInAlias String [String] String [String] Type.Raw
    | UnboundTypeVarsInUnion String [String] String [String] [(String, [Type.Raw])]


-- TO REPORT

toReport :: RenderType.Localizer -> Error -> Report.Report
toReport localizer err =
  case err of
    Parse messages ->
        parseErrorReport messages

    BadFunctionName arity ->
        Report.simple
          "BAD FUNCTION DEFINITION"
          "Complex patterns cannot be used as function names."
          ( "This function definition has " ++ show arity ++ " arguments, but instead of a normal name like\n"
            ++ "`add` or `reverse` it has a pattern. There is no way to \"deconstruct\" a\n"
            ++ "function with pattern matching, so this needs to be changed to a normal name."
          )

    BadPattern name ->
        Report.simple
          "BAD PATTERN"
          ("The free variable `" ++ name ++ "` appears more than once in this pattern.")
          ( "Rename the variables so there are no duplicates.\n\n"
            ++ "In Elm, pattern matching works by binding these free variables to subsections\n"
            ++ "of the matching value. It does not make sense to have the same name for two\n"
            ++ "different subsections though! When you say `" ++ name ++ "` in some code, there\n"
            ++ "is no way for me to know which subsection you mean!"
          )

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
          ("There is a type annotation for `" ++ valueName ++ "` but there"
            ++ " is no corresponding definition!"
          )
          ("Directly below the type annotation, put a definition like:\n\n"
            ++ "    " ++ valueName ++ " = 42"
          )

    PortWithoutAnnotation portName ->
        Report.simple
          "PORT ERROR"
          ("Port `" ++ portName ++ "` does not have a type annotation!")
          ("Directly above the port definition, I need something like this:\n\n"
            ++ "    port " ++ portName ++ " : Signal Int"
          )


    UnexpectedPort ->
        Report.simple
          "PORT ERROR"
          "This module has ports, but ports can only appear in the main module."
          ( "Ports in library code would create hidden dependencies where importing a\n"
            ++ "module could bring in constraints not captured in the public API. Furthermore,\n"
            ++ "if the module is imported twice, do we send values out the port twice?"
          )

    DuplicateFieldName name ->
        Report.simple
          "DUPLICATE FIELD"
          ("This record has more than one field named `" ++ name ++ "`.")
          "There can only be one. Do some renaming to make sure the names are distinct!"

    DuplicateValueDeclaration name ->
        Report.simple
          "DUPLICATE DEFINITION"
          ("Naming multiple top-level values `" ++ name ++ "` makes things\n"
            ++ "ambiguous. When you say `" ++ name ++ "` which one do you want?"
          )
          ("Find all the top-level values named `" ++ name ++ "` and\n"
            ++ "do some renaming. Make sure the names are distinct!"
          )

    DuplicateTypeDeclaration name ->
        Report.simple
          "DUPLICATE DEFINITION"
          ("Naming multiple types `" ++ name ++ "` makes things ambiguous\n"
            ++ "When you say `" ++ name ++ "` which one do you want?"
          )
          ("Find all the types named `" ++ name ++ "` and\n"
            ++ "do some renaming. Make sure the names are distinct!"
          )

    DuplicateDefinition name ->
        Report.simple
          "DUPLICATE DEFINITION"
          ("Naming multiple values `" ++ name ++ "` in a single let-expression makes\n"
            ++ "things ambiguous. When you say `" ++ name ++ "` which one do you want?"
          )
          ("Find all the values named `" ++ name ++ "` in this let-expression and\n"
            ++ "do some renaming. Make sure the names are distinct!"
          )

    UnboundTypeVarsInAlias typeName givenVars tvar tvars tipe ->
        unboundTypeVars typeName tvar tvars $
            RenderType.aliasDecl localizer typeName (givenVars ++ tvar : tvars) tipe

    UnboundTypeVarsInUnion typeName givenVars tvar tvars ctors ->
        unboundTypeVars typeName tvar tvars $
            RenderType.typeDecl localizer typeName (givenVars ++ tvar : tvars) ctors


unboundTypeVars :: String -> String -> [String] -> String -> Report.Report
unboundTypeVars typeName tvar tvars revisedDeclaration =
  Report.simple
    "UNBOUND TYPE VARS"
    ( "Not all type variables in `" ++ typeName ++ "` are listed, making sneaky\n"
      ++ "type errors possible. Unbound type variables include: "
      ++ List.intercalate ", " (tvar:tvars)
    )
    ( "You probably want this definition instead:\n"
      ++ concatMap ("\n    "++) (lines revisedDeclaration) ++ "\n\n"
      ++ "Here's why. Imagine one `" ++ typeName ++ "` where `" ++ tvar ++ "` is an Int and\n"
      ++ "another where it is a Bool. When we explicitly list the type variables, type\n"
      ++ "checker can see that they are actually different types."
    )


-- TAGGING PARSE ERRORS

newline :: String
newline = "NEWLINE"

freshLine :: String
freshLine = "FRESH_LINE"

whitespace :: String
whitespace = "WHITESPACE"

keyword :: String -> String
keyword kwd =
  "KEYWORD=" ++ kwd

unkeyword :: String -> Maybe String
unkeyword message =
  if List.isPrefixOf "KEYWORD=" message
    then Just (drop (length "KEYWORD=") message)
    else Nothing


-- REPORTING PARSE ERRORS

parseErrorReport :: [Parsec.Message] -> Report.Report
parseErrorReport messages =
  let
    addMsg message hint =
      case message of
        Parsec.SysUnExpect _msg ->
            hint

        Parsec.UnExpect _msg ->
            hint

        Parsec.Expect msg ->
          let
            msg' =
              if msg `elem` [whitespace, newline, freshLine]
                then "whitespace"
                else msg
          in
            hint { _expected = Set.insert msg' (_expected hint) }

        Parsec.Message msg ->
            hint { _messages = msg : _messages hint }

    (ParseHint msgs expects) =
      foldr addMsg emptyHint messages

    preHint =
      case msgs of
        [msg] ->
            case unkeyword msg of
              Just kwd ->
                  "It looks like the keyword `" ++ kwd ++ "` is being used as a variable.\n"
                  ++ "Try renaming it to something else."
              Nothing ->
                  msg

        _ -> "I ran into something unexpected when parsing your code!"

    postHint =
      if Set.null expects
        then ""
        else
          "I am looking for one of the following things:\n"
          ++ concatMap ("\n    "++) (Set.toList expects)
  in
    Report.simple "SYNTAX PROBLEM" preHint postHint


data ParseHint = ParseHint
    { _messages :: [String]
    , _expected :: Set.Set String
    }
    deriving (Show)


emptyHint :: ParseHint
emptyHint =
  ParseHint [] Set.empty
