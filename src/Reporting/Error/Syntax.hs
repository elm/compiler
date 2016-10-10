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

    | CommentOnNothing

    | SettingsOnNormalModule
    | SettingsOnPortModule
    | DuplicateSettingOnEffectModule String
    | BadSettingOnEffectModule String
    | NoSettingsOnEffectModule
    | MissingManagerOnEffectModule String
    | UnexpectedPort String

    | InfixDuplicate String
    | TypeWithoutDefinition String

    | DuplicateArgument String String
    | DuplicateFieldName String
    | DuplicateValueDeclaration String
    | DuplicateTypeDeclaration String
    | DuplicateDefinition String
    | UnboundTypeVarsInUnion String [String] [String]
    | UnboundTypeVarsInAlias String [String] [String]
    | UnusedTypeVarsInAlias String [String] [String]
    | MessyTypeVarsInAlias String [String] [String] [String]


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

    CommentOnNothing ->
        Report.report
          "STRAY COMMENT"
          Nothing
          ("This documentation comment is not followed by anything.")
          ( Help.reflowParagraph $
              "All documentation comments need to be right above the declaration they\
              \ describe. Maybe some code got deleted or commented out by accident? Or\
              \ maybe this comment is here by accident?"
          )

    SettingsOnNormalModule ->
        Report.report
          "BAD MODULE DECLARATION"
          Nothing
          "A normal module can expose values, but not settings like this."
          ( Help.reflowParagraph $
              "If you want a normal module, just remove this stuff. If you want to create\
              \ an `effect module` you just forgot to use the `effect` keyword. In that\
              \ case, just change `module` to `effect module` and you should be headed in\
              \ the right direction!"
          )

    SettingsOnPortModule ->
        Report.report
          "BAD MODULE DECLARATION"
          Nothing
          "A port module can expose values, but not have settings like this."
          ( Help.reflowParagraph $
              "If you want a port module, just remove this stuff. If you want to create\
              \ a custom effect module instead (which is rare) just change `port module`\
              \ to `effect module` and you should be headed in the right direction!"
          )

    DuplicateSettingOnEffectModule name ->
        Report.report
          "EFFECT MODULE PROBLEM"
          Nothing
          ("You have defined " ++ Help.functionName name ++ " multiple times.")
          ( Help.reflowParagraph $
              "There can only be one " ++ Help.functionName name
              ++ " though! Remove until there is one left."
          )

    BadSettingOnEffectModule name ->
        Report.report
          "EFFECT MODULE PROBLEM"
          Nothing
          ("Setting " ++ Help.functionName name ++ " is not recognized.")
          ( Help.reflowParagraph $
              "Remove this entry and you should be all set!"
          )

    NoSettingsOnEffectModule ->
        Report.report
          "EFFECT MODULE PROBLEM"
          Nothing
          ("You are defining an effect module, but it has no settings.")
          ( Help.reflowParagraph $
              "If you just wanted a normal module, change the keywords `effect module`\
              \ to `module` and you should be all set. If you want a proper effect module,\
              \ you need to specify your commands and/or subscriptions. Read more about this\
              \ here: <http://guide.elm-lang.org/effect_managers/>"
          )

    MissingManagerOnEffectModule name ->
        Report.report
          "EFFECT MODULE PROBLEM"
          Nothing
          ("You are defining an effect module, but there is no " ++ Help.functionName name ++ " defined.")
          ( Help.reflowParagraph $
              "There is a small set of top-level functions and values that must be defined\
              \ in any complete effect module. The best thing is probably to just read more\
              \ about effect modules here:\
              \ <http://guide.elm-lang.org/effect_managers/>"
          )

    UnexpectedPort name ->
        Report.report
          "BAD PORT"
          Nothing
          ("You are declaring port " ++ Help.functionName name ++ " in a normal module.")
          ( Help.reflowParagraph $
              "All ports must be defined in a `port module`. You should probably have just\
              \ one of these for your project. This way all of your foreign interactions\
              \ stay relatively organized."
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

    DuplicateArgument funcName argName ->
        Report.report
          "DUPLICATE ARGUMENT"
          Nothing
          ( "The name `" ++ argName
            ++ "` is used more than once in the arguments of `"
            ++ funcName ++ "`."
          )
          ( Help.reflowParagraph $
              "Rename things until `" ++ argName ++ "` is used only once.\
              \ Otherwise how can we tell which one you want when you\
              \ say `" ++ argName ++ "` it in the body of your function?"
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

    UnboundTypeVarsInUnion typeName givenVars unbound ->
        unboundTypeVars "type" typeName givenVars unbound

    UnboundTypeVarsInAlias typeName givenVars unbound ->
        unboundTypeVars "type alias" typeName givenVars unbound

    UnusedTypeVarsInAlias typeName givenVars unused ->
        Report.report
          "UNUSED TYPE VARIABLES"
          Nothing
          ( "Type alias `" ++ typeName ++ "` cannot have unused type variables: "
            ++ Help.commaSep unused
          )
          ( Help.stack
              [ text "You probably need to change the declaration like this:"
              , dullyellow $ hsep $
                  map text ("type" : "alias" : typeName : filter (`notElem` unused) givenVars ++ ["=", "..."])
              ]
          )

    MessyTypeVarsInAlias typeName givenVars unused unbound ->
        Report.report
          "TYPE VARIABLE PROBLEMS"
          Nothing
          ( "Type alias `" ++ typeName ++ "` has some problems with type variables."
          )
          ( Help.stack
              [ Help.reflowParagraph $
                  "The declaration says it uses certain type variables ("
                  ++ Help.commaSep unused ++ ") but they do not appear in the aliased type. "
                  ++ "Furthermore, the aliased type says it uses type variables ("
                  ++ Help.commaSep unbound
                  ++ ") that do not appear in the declaration."
              , text "You probably need to change the declaration like this:"
              , dullyellow $ hsep $
                  map text ("type" : "alias" : typeName : filter (`notElem` unused) givenVars ++ unbound ++ ["=", "..."])
              ]
          )


unboundTypeVars :: String -> String -> [String] -> [String] -> Report.Report
unboundTypeVars declKind typeName givenVars unboundVars =
  Report.report
    "UNBOUND TYPE VARIABLES"
    Nothing
    ( Help.capitalize declKind ++ " `" ++ typeName ++ "` must declare its use of type variable"
      ++ Help.commaSep unboundVars
    )
    ( Help.stack
        [ text "You probably need to change the declaration like this:"
        , hsep $
            map text (declKind : typeName : givenVars)
            ++ map (dullyellow . text) unboundVars
            ++ map text ["=", "..."]
        , Help.reflowParagraph $
            "Here's why. Imagine one `" ++ typeName ++ "` where `" ++ head unboundVars ++
            "` is an Int and another where it is a Bool. When we explicitly list the type\
            \ variables, the type checker can see that they are actually different types."
        ]
    )



-- TAGGING PARSE ERRORS


newline :: String
newline = "NEWLINE"


freshLine :: String
freshLine = "FRESH_LINE"


whitespace :: String
whitespace = "WHITESPACE"


tab :: String
tab = "TAB"


keyword :: String -> String
keyword kwd =
  "KEYWORD=" ++ kwd


prime :: String -> String
prime var =
  "PRIME=" ++ var


data SpecialMessage
  = MsgKeyword String
  | MsgPrime String
  | MsgTab


extractSpecialMessage :: String -> Maybe SpecialMessage
extractSpecialMessage message =
  if List.isPrefixOf "KEYWORD=" message then
      Just $ MsgKeyword (drop (length "KEYWORD=") message)

  else if List.isPrefixOf "PRIME=" message then
      Just $ MsgPrime (drop (length "PRIME=") message)

  else if tab == message then
      Just MsgTab

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
            if msg == tab then
              -- Our parser looks for tab so it can throw a custom tab error message.
              -- Exclude tabs from the list of expected tokens so that no one thinks they are supported.
              hint
            else
              hint { _expected = Set.insert msg' (_expected hint) }

        Parsec.Message msg ->
            hint { _messages = msg : _messages hint }

    (ParseHint msgs expects) =
      foldr addMsg emptyHint messages

    (preHint, maybePost) =
      case msgs of
        [msg] ->
            case extractSpecialMessage msg of
              Just (MsgKeyword kwd) ->
                  ( "It looks like the keyword `" ++ kwd ++ "` is being used as a variable."
                  , Just "Rename it to something else."
                  )

              Just (MsgPrime var) ->
                  ( "Ran into a single quote in a variable name. This was removed in 0.18!"
                  , Just $
                      "Change it to a number or an underscore, like " ++ var ++ "_ or " ++ var
                      ++ "1\n\nOr better yet, choose a more descriptive name!"
                  )

              Just MsgTab ->
                  ( "A tab character was found, but all whitespace (including indentation) must be spaces not tabs."
                  , Just "I am looking for spaces, not tabs."
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
