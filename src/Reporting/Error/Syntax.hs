{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Reporting.Error.Syntax where

import Data.Text (Text)

import qualified Reporting.Render.Type as RenderType
import qualified Reporting.Region as Region
import qualified Reporting.Report as Report
import qualified Reporting.Helpers as Help
import Reporting.Helpers ((<>), dullyellow, hsep, i2t, text)


data Error
    = Parse
    | BadFunctionName Int
    | BadPattern Text

    | CommentOnNothing

    | SettingsOnNormalModule
    | SettingsOnPortModule
    | DuplicateSettingOnEffectModule Text
    | BadSettingOnEffectModule Text
    | NoSettingsOnEffectModule
    | MissingManagerOnEffectModule Text
    | UnexpectedPort Text

    | TypeWithoutDefinition Text

    | DuplicateArgument Text Text
    | DuplicateFieldName Region.Region Text
    | DuplicateValueDeclaration Text
    | DuplicateTypeDeclaration Text
    | DuplicateDefinition Text
    | UnboundTypeVarsInUnion Text [Text] [Text]
    | UnboundTypeVarsInAlias Text [Text] [Text]
    | UnusedTypeVarsInAlias Text [Text] [Text]
    | MessyTypeVarsInAlias Text [Text] [Text] [Text]
    deriving (Show)



-- TO REPORT


toReport :: RenderType.Localizer -> Error -> Report.Report
toReport _localizer err =
  case err of
    Parse ->
        Report.report "PARSE ERROR" Nothing "Something is wrong." (text "TODO")

    BadFunctionName arity ->
        Report.report
          "BAD FUNCTION DEFINITION"
          Nothing
          "Complex patterns cannot be used as function names."
          ( Help.reflowParagraph $
              "This function definition has " <> i2t arity <> " arguments, but instead\
              \ of a normal name like `add` or `reverse` it has a pattern. There is no\
              \ way to \"deconstruct\" a function with pattern matching, so this needs to\
              \ be changed to a normal name."
          )

    BadPattern name ->
        Report.report
          "BAD PATTERN"
          Nothing
          ("The free variable `" <> name <> "` appears more than once in this pattern.")
          ( Help.stack
              [ text "Rename the variables so there are no duplicates."
              , Help.reflowParagraph $
                  "In Elm, pattern matching works by binding these free variables to subsections\
                  \ of the matching value. It does not make sense to have the same name for two\
                  \ different subsections though! When you say `" <> name <> "` in some code, there\
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
          ("You have defined " <> Help.functionName name <> " multiple times.")
          ( Help.reflowParagraph $
              "There can only be one " <> Help.functionName name
              <> " though! Remove until there is one left."
          )

    BadSettingOnEffectModule name ->
        Report.report
          "EFFECT MODULE PROBLEM"
          Nothing
          ("Setting " <> Help.functionName name <> " is not recognized.")
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
          ("You are defining an effect module, but there is no " <> Help.functionName name <> " defined.")
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
          ("You are declaring port " <> Help.functionName name <> " in a normal module.")
          ( Help.reflowParagraph $
              "All ports must be defined in a `port module`. You should probably have just\
              \ one of these for your project. This way all of your foreign interactions\
              \ stay relatively organized."
          )

    TypeWithoutDefinition valueName ->
        Report.report
          "MISSING DEFINITION"
          Nothing
          ("There is a type annotation for `" <> valueName <> "` but there"
            <> " is no corresponding definition!"
          )
          ( text $
              "Directly below the type annotation, put a definition like:\n\n"
              <> "    " <> valueName <> " = 42"
          )

    DuplicateArgument funcName argName ->
        Report.report
          "DUPLICATE ARGUMENT"
          Nothing
          ( "The name `" <> argName
            <> "` is used more than once in the arguments of "
            <> Help.functionName funcName <> "."
          )
          ( Help.reflowParagraph $
              "Rename things until `" <> argName <> "` is used only once.\
              \ Otherwise how can we tell which one you want when you\
              \ say `" <> argName <> "` it in the body of your function?"
          )

    DuplicateFieldName region name ->
        Report.report
          "DUPLICATE FIELD"
          (Just region)
          ("This record has more than one field named `" <> name <> "`.")
          (text "There can only be one. Do some renaming to make sure the names are distinct!")

    DuplicateValueDeclaration name ->
        Report.report
          "DUPLICATE DEFINITION"
          Nothing
          ( "Naming multiple top-level values " <> Help.functionName name <> " makes things\n"
            <> "ambiguous. When you say " <> Help.functionName name <> " which one do you want?"
          )
          ( Help.reflowParagraph $
              "Find all the top-level values named " <> Help.functionName name
              <> " and do some renaming. Make sure the names are distinct!"
          )

    DuplicateTypeDeclaration name ->
        Report.report
          "DUPLICATE DEFINITION"
          Nothing
          ( "There are multiple types named `" <> name <> "` in this module."
          )
          ( Help.reflowParagraph $
              "Search through this module, find all the types named `"
              <> name <> "`, and give each of them a unique name."
          )

    DuplicateDefinition name ->
        Report.report
          "DUPLICATE DEFINITION"
          Nothing
          ("There are multiple values named `" <> name <> "` in this let-expression."
          )
          ( Help.reflowParagraph $
              "Search through this let-expression, find all the values named `"
              <> name <> "`, and give each of them a unique name."
          )

    UnboundTypeVarsInUnion typeName givenVars unbound ->
        unboundTypeVars "type" typeName givenVars unbound

    UnboundTypeVarsInAlias typeName givenVars unbound ->
        unboundTypeVars "type alias" typeName givenVars unbound

    UnusedTypeVarsInAlias typeName givenVars unused ->
        Report.report
          "UNUSED TYPE VARIABLES"
          Nothing
          ( "Type alias `" <> typeName <> "` cannot have unused type variables: "
            <> Help.commaSep unused
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
          ( "Type alias `" <> typeName <> "` has some problems with type variables."
          )
          ( Help.stack
              [ Help.reflowParagraph $
                  "The declaration says it uses certain type variables ("
                  <> Help.commaSep unused <> ") but they do not appear in the aliased type. "
                  <> "Furthermore, the aliased type says it uses type variables ("
                  <> Help.commaSep unbound
                  <> ") that do not appear in the declaration."
              , text "You probably need to change the declaration like this:"
              , dullyellow $ hsep $
                  map text ("type" : "alias" : typeName : filter (`notElem` unused) givenVars ++ unbound ++ ["=", "..."])
              ]
          )


unboundTypeVars :: Text -> Text -> [Text] -> [Text] -> Report.Report
unboundTypeVars declKind typeName givenVars unboundVars =
  Report.report
    "UNBOUND TYPE VARIABLES"
    Nothing
    ( Help.capitalize declKind <> " `" <> typeName <> "` must declare its use of type variable"
      <> Help.commaSep unboundVars
    )
    ( Help.stack
        [ text "You probably need to change the declaration like this:"
        , hsep $
            map text (declKind : typeName : givenVars)
            ++ map (dullyellow . text) unboundVars
            ++ map text ["=", "..."]
        , Help.reflowParagraph $
            "Here's why. Imagine one `" <> typeName <> "` where `" <> head unboundVars <>
            "` is an Int and another where it is a Bool. When we explicitly list the type\
            \ variables, the type checker can see that they are actually different types."
        ]
    )
