{-# OPTIONS_GHC -Wall #-}
module Reporting.Error.Syntax where

import qualified Data.List as List
import qualified Data.Set as Set
import qualified Text.Parsec.Error as Parsec
import Text.PrettyPrint.ANSI.Leijen (dullyellow, hsep, text)

import qualified Reporting.Error.Helpers as Help
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
    | UnboundTypeVarsInAlias String [String] String [String]
    | UnboundTypeVarsInUnion String [String] String [String]


-- TO REPORT

toReport :: RenderType.Localizer -> Error -> Report.Report
toReport _localizer err =
  case err of
    Parse messages ->
        parseErrorReport messages

    BadFunctionName arity ->
        Report.report
          "BAD FUNCTION DEFINITION"
          Nothing
          "Complex patterns cannot be used as function names."
          ( Help.reflowParagraph $
              "This function definition has " ++ show arity ++ " arguments, but instead\
              \ of a normal name like `add` or `reverse` it has a pattern. There is no\
              \ way to \"deconstruct\" a function with pattern matching, so this needs to\
              \ be changed to a normal name."
          )

    BadPattern name ->
        Report.report
          "BAD PATTERN"
          Nothing
          ("The free variable `" ++ name ++ "` appears more than once in this pattern.")
          ( Help.stack
              [ text "Rename the variables so there are no duplicates."
              , Help.reflowParagraph $
                  "In Elm, pattern matching works by binding these free variables to subsections\
                  \ of the matching value. It does not make sense to have the same name for two\
                  \ different subsections though! When you say `" ++ name ++ "` in some code, there\
                  \ is no way for me to know which subsection you mean!"
              ]
          )

    InfixDuplicate opName ->
        Report.report
          "INFIX OVERLAP"
          Nothing
          ("The infix declarations for " ++ Help.functionName opName ++ " must be removed.")
          ( Help.reflowParagraph $
              "The precedence and associativity of a particular operator can only be set in\
              \ one place in a code base, and " ++ Help.functionName opName
              ++ " has already been set somewhere else."
          )

    TypeWithoutDefinition valueName ->
        Report.report
          "MISSING DEFINITION"
          Nothing
          ("There is a type annotation for `" ++ valueName ++ "` but there"
            ++ " is no corresponding definition!"
          )
          ( text $
              "Directly below the type annotation, put a definition like:\n\n"
              ++ "    " ++ valueName ++ " = 42"
          )

    PortWithoutAnnotation portName ->
        Report.report
          "PORT ERROR"
          Nothing
          ("Port `" ++ portName ++ "` does not have a type annotation!")
          ( text $
              "Directly above the port definition, I need something like this:\n\n"
              ++ "    port " ++ portName ++ " : Signal Int"
          )


    UnexpectedPort ->
        Report.report
          "PORT ERROR"
          Nothing
          "This module has ports, but ports can only appear in the main module."
          ( Help.reflowParagraph $
              "Ports in library code would create hidden dependencies where importing a\
              \ module could bring in constraints not captured in the public API. Furthermore,\
              \ if the module is imported twice, do we send values out the port twice?"
          )

    DuplicateFieldName name ->
        Report.report
          "DUPLICATE FIELD"
          Nothing
          ("This record has more than one field named `" ++ name ++ "`.")
          (text "There can only be one. Do some renaming to make sure the names are distinct!")

    DuplicateValueDeclaration name ->
        Report.report
          "DUPLICATE DEFINITION"
          Nothing
          ( "Naming multiple top-level values `" ++ name ++ "` makes things\n"
            ++ "ambiguous. When you say `" ++ name ++ "` which one do you want?"
          )
          ( Help.reflowParagraph $
              "Find all the top-level values named " ++ Help.functionName name
              ++ " and do some renaming. Make sure the names are distinct!"
          )

    DuplicateTypeDeclaration name ->
        Report.report
          "DUPLICATE DEFINITION"
          Nothing
          ( "There are multiple types named `" ++ name ++ "` in this module."
          )
          ( Help.reflowParagraph $
              "Search through this module, find all the types named `" ++ name
              ++ "`, and give each of them a unique name."
          )

    DuplicateDefinition name ->
        Report.report
          "DUPLICATE DEFINITION"
          Nothing
          ("There are multiple values named `" ++ name ++ "` in this let-expression."
          )
          ( Help.reflowParagraph $
              "Search through this let-expression, find all the values named `" ++ name
              ++ "`, and give each of them a unique name."
          )

    UnboundTypeVarsInAlias typeName givenVars tvar tvars ->
        unboundTypeVars "type alias" typeName givenVars tvar tvars

    UnboundTypeVarsInUnion typeName givenVars tvar tvars ->
        unboundTypeVars "type" typeName givenVars tvar tvars



unboundTypeVars :: String -> String -> [String] -> String -> [String] -> Report.Report
unboundTypeVars declKind typeName givenVars tvar tvars =
  Report.report
    "UNBOUND TYPE VARIABLES"
    Nothing
    ( Help.capitalize declKind ++ " `" ++ typeName ++ "` must declare its use of type variable"
      ++ Help.commaSep (tvar:tvars)
    )
    ( Help.stack
        [ text "You probably need to change the declaration like this:"
        , hsep $
            map text (declKind : typeName : givenVars)
            ++ map (dullyellow . text) (tvar : tvars)
            ++ map text ["=", "…"]
        , Help.reflowParagraph $
            "Here's why. Imagine one `" ++ typeName ++ "` where `" ++ tvar ++ "` is an Int and\
            \ another where it is a Bool. When we explicitly list the type variables, type\
            \ checker can see that they are actually different types."
        ]
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
  if List.isPrefixOf "KEYWORD=" message then
      Just (drop (length "KEYWORD=") message)

  else
      Nothing


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
              if msg `elem` [whitespace, newline, freshLine] then
                  "whitespace"
              else
                  msg
          in
            hint { _expected = Set.insert msg' (_expected hint) }

        Parsec.Message msg ->
            hint { _messages = msg : _messages hint }

    (ParseHint msgs expects) =
      foldr addMsg emptyHint messages

    (preHint, maybePost) =
      case msgs of
        [msg] ->
            case unkeyword msg of
              Just kwd ->
                  ( "It looks like the keyword `" ++ kwd ++ "` is being used as a variable."
                  , Just "Rename it to something else."
                  )

              Nothing ->
                  ( msg
                  , Nothing
                  )

        _ ->
            ( "I ran into something unexpected when parsing your code!"
            , Nothing
            )

    postHint =
      if Set.null expects then
          "Maybe <http://elm-lang.org/docs/syntax> can help you figure it out."

      else
          "I am looking for one of the following things:\n"
          ++ concatMap ("\n    "++) (Set.toList expects)
  in
    Report.report
      "SYNTAX PROBLEM"
      Nothing
      preHint
      (text (maybe postHint id maybePost))


data ParseHint = ParseHint
    { _messages :: [String]
    , _expected :: Set.Set String
    }
    deriving (Show)


emptyHint :: ParseHint
emptyHint =
  ParseHint [] Set.empty
