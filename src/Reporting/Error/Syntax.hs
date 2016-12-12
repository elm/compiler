{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Reporting.Error.Syntax
  ( Error(..)
  , ParseError(..)
  , Problem(..), Theory(..)
  , toReport
  )
  where

import qualified Data.Set as Set
import Data.Text (Text)

import qualified Reporting.Render.Type as RenderType
import qualified Reporting.Region as Region
import qualified Reporting.Report as Report
import qualified Reporting.Helpers as Help
import Reporting.Helpers ((<>), dullyellow, hsep, i2t, reflowParagraph, text)



-- ALL SYNTAX ERRORS


data Error
    = Parse Problem
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



-- PARSE ERRORS


data ParseError
  = ParseError !Int !Int !Problem


data Problem
  = Tab
  | EndOfFile_Comment
  | EndOfFile_Shader
  | EndOfFile_String
  | EndOfFile_MultiString
  | NewLineInString
  | BadEscape
  | BadChar
  | BadNumberDot
  | BadNumberEnd
  | BadNumberExp
  | BadNumberHex
  | BadNumberZero
  | BadShader Text
  | BadChunkInQualifiedCapVar
  | HasType
  | Equals
  | Arrow
  | Pipe
  | Dot
  | Theories [Theory]


data Theory
  = Keyword Text
  | Symbol Text
  | Variable
  | InfixOp
  | Digit
  | BadIndent
  | FreshLine
  | Expecting Text
  deriving (Eq, Ord)



-- TO REPORT


toReport :: RenderType.Localizer -> Error -> Report.Report
toReport _localizer err =
  case err of
    Parse problem ->
        problemToReport problem

    BadFunctionName arity ->
        Report.report
          "BAD FUNCTION DEFINITION"
          Nothing
          "Complex patterns cannot be used as function names."
          ( reflowParagraph $
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
              , reflowParagraph $
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
          ( reflowParagraph $
              "All documentation comments need to be right above the declaration they\
              \ describe. Maybe some code got deleted or commented out by accident? Or\
              \ maybe this comment is here by accident?"
          )

    SettingsOnNormalModule ->
        Report.report
          "BAD MODULE DECLARATION"
          Nothing
          "A normal module can expose values, but not settings like this."
          ( reflowParagraph $
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
          ( reflowParagraph $
              "If you want a port module, just remove this stuff. If you want to create\
              \ a custom effect module instead (which is rare) just change `port module`\
              \ to `effect module` and you should be headed in the right direction!"
          )

    DuplicateSettingOnEffectModule name ->
        Report.report
          "EFFECT MODULE PROBLEM"
          Nothing
          ("You have defined " <> Help.functionName name <> " multiple times.")
          ( reflowParagraph $
              "There can only be one " <> Help.functionName name
              <> " though! Remove until there is one left."
          )

    BadSettingOnEffectModule name ->
        Report.report
          "EFFECT MODULE PROBLEM"
          Nothing
          ("Setting " <> Help.functionName name <> " is not recognized.")
          ( reflowParagraph $
              "Remove this entry and you should be all set!"
          )

    NoSettingsOnEffectModule ->
        Report.report
          "EFFECT MODULE PROBLEM"
          Nothing
          ("You are defining an effect module, but it has no settings.")
          ( reflowParagraph $
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
          ( reflowParagraph $
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
          ( reflowParagraph $
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
          ( reflowParagraph $
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
          ( reflowParagraph $
              "Find all the top-level values named " <> Help.functionName name
              <> " and do some renaming. Make sure the names are distinct!"
          )

    DuplicateTypeDeclaration name ->
        Report.report
          "DUPLICATE DEFINITION"
          Nothing
          ( "There are multiple types named `" <> name <> "` in this module."
          )
          ( reflowParagraph $
              "Search through this module, find all the types named `"
              <> name <> "`, and give each of them a unique name."
          )

    DuplicateDefinition name ->
        Report.report
          "DUPLICATE DEFINITION"
          Nothing
          ("There are multiple values named `" <> name <> "` in this let-expression."
          )
          ( reflowParagraph $
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
              [ reflowParagraph $
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
        , reflowParagraph $
            "Here's why. Imagine one `" <> typeName <> "` where `" <> head unboundVars <>
            "` is an Int and another where it is a Bool. When we explicitly list the type\
            \ variables, the type checker can see that they are actually different types."
        ]
    )



-- PARSE ERROR TO REPORT


parseReport :: Text -> Help.Doc -> Report.Report
parseReport pre post =
  Report.report "PARSE ERROR" Nothing pre post


problemToReport :: Problem -> Report.Report
problemToReport problem =
  case problem of
    Tab ->
      parseReport
        "I ran into a tab, but tabs are not allowed in Elm files."
        (reflowParagraph "Replace the tab with spages.")

    EndOfFile_Comment ->
      parseReport
        "I got to the end of the file while parsing a multi-line comment."
        ( Help.stack
            [ reflowParagraph $
                "Multi-line comments look like {- comment -}, and it looks like\
                \ you are missing the closing marker."
            , reflowParagraph $
                "Nested multi-line comments like {- this {- and this -} -} are allowed.\
                \ That means the opening and closing markers must be balanced, just\
                \ like parentheses in normal code. Maybe that is the problem?"
            ]
        )

    EndOfFile_Shader ->
      parseReport
        "I got to the end of the file while parsing a GLSL block."
        (reflowParagraph $
          "A shader should be defined in a block like this: [glsl| ... |]"
        )

    EndOfFile_String ->
      parseReport
        "I got to the end of the file while parsing a string."
        (reflowParagraph $
          "Strings look like \"this\" with double quotes on each end.\
          \ So the closing double quote seems to be missing."
        )

    EndOfFile_MultiString ->
      parseReport
        "I got to the end of the file while parsing a multi-line string."
        (reflowParagraph $
          "Multi-line strings begin and end with three double quotes in a\
          \ row, like \"\"\"this\"\"\". So the closing marker seems\
          \ to be missing."
        )

    NewLineInString ->
      parseReport
        "This string is missing the closing quote."
        ( Help.stack
            [ reflowParagraph $
                "Elm strings must start and end with a double quote, like \"this\"."
            , reflowParagraph $
                "If you want a string that can contain newlines, use three double quotes in a\
                \ row, like \"\"\"this\"\"\"."
            ]
        )

    BadEscape ->
      parseReport
        "Ran into a bad escape code."
        ( reflowParagraph
            "Elm allows typical escape characters like \\n and \\t, but you can\
            \ also use \\x0040 to refer to unicode characters by their hex code.\
            \ It seems like something has gone wrong with one of these codes."
        )

    BadChar ->
      parseReport
        "Ran into a bad use of single quotes."
        ( Help.stack
            [ reflowParagraph $
                "If you want to create a string, just switch to double quotes:"
            , Help.indent 4 $
                dullyellow (text "'this'")
                <> text " => "
                <> dullyellow (text "\"this\"")
            , reflowParagraph $
                "Unlike JavaScript, Elm distinguishes between strings like \"hello\"\
                \ and individual characters like 'A' and '3'. It is not all strings!\
                \ If you really do want a character though, something went wrong\
                \ and I did not find the closing single quote."
            ]
        )

    BadNumberDot ->
      parseReport
        "Numbers cannot end with a decimal points."
        ( reflowParagraph $
            "Leave it off or add some digits after it, like or 3 or 3.0"
        )

    BadNumberEnd ->
      parseReport
        "Numbers cannot have letters or underscores in them."
        ( reflowParagraph $
            "Maybe a space is missing between a number and a variable?"
        )

    BadNumberExp ->
      parseReport
        "If you put the letter E in a number, it should followed by more digits."
        ( reflowParagraph $
            "If you want to say 1000, you can also say 1e3.\
            \ You cannot just end it with an E though!"
        )

    BadNumberHex ->
      parseReport
        "I see the start of a hex number, but not the end."
        ( reflowParagraph $
            "A hex number looks like 0x123ABC, where the 0x is followed by hexidecimal digits (i.e. 0123456789abcdefABCDEF)"
        )

    BadNumberZero ->
      parseReport
        "Normal numbers cannot start with a zero. Take the zeros off the front."
        ( reflowParagraph $
            "Only numbers like 0x0040 or 0.25 can start with a zero."
        )

    BadShader msg ->
      parseReport
        "I ran into a problem while parsing this GLSL block."
        (reflowParagraph msg)

    BadChunkInQualifiedCapVar ->
      error "TODO"

    HasType ->
      parseReport
        "Single colons should only show up in types."
        ( reflowParagraph $
            "Maybe you want :: instead? Or maybe you are defining\
            \ a type annotation but there are extra spaces before it?"
        )

    Equals ->
      parseReport
        "The = operator is reserved for defining variables."
        ( reflowParagraph $
            "Maybe you want == instead? Or maybe you are defining a\
            \ variable but there are extra spaces before it?"
        )

    Arrow ->
      parseReport
        "Arrows are reserved for cases and anonymous functions."
        ( reflowParagraph $
            "Maybe you want > or >= instead?"
        )

    Pipe ->
      parseReport
        "Vertical bars are reserved for use in type declarations."
        ( reflowParagraph $
            "Maybe you want || instead?"
        )

    Dot ->
      parseReport
        "Dots are for record access and decimal points."
        ( reflowParagraph $
            "They cannot float around on their own though, so\
            \ maybe there is some extra whitespace?"
        )

    Theories allTheories ->
      case Set.toList (Set.fromList allTheories) of
        [] ->
          parseReport
            "Something went wrong when parsing this code."
            ( reflowParagraph $
                "I do not have any suggestions though, and I suspect there is some\
                \ sort of bug. Can you get it down to an <http://sscce.org> and\
                \ share it at <https://github.com/elm-lang/error-message-catalog/issues>?\
                \ That way we can figure out how to give better advice!"
            )

        theories ->
          error "TODO" (map theoryToText theories)


theoryToText :: Theory -> Text
theoryToText theory =
  case theory of
    Keyword _ ->
      ""

    Symbol _ ->
      ""

    Variable ->
      ""

    InfixOp ->
      ""

    Digit ->
      "a digit, like 0 or 6"

    BadIndent ->
      ""

    FreshLine ->
      ""

    Expecting _ ->
      ""
