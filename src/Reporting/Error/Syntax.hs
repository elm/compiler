{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Reporting.Error.Syntax
  ( Error(..)
  , ParseError(..)
  , Problem(..), Theory(..)
  , EscapeProblem(..)
  , ContextStack, Context(..)
  , BadOp(..), Next(..)
  , toReport
  )
  where

import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Text (Text)

import qualified Reporting.Render.Type as RenderType
import qualified Reporting.Region as R
import qualified Reporting.Report as Report
import qualified Reporting.Helpers as Help
import Reporting.Helpers ((<>), dullyellow, green, hsep, i2t, reflowParagraph, text)



-- ALL SYNTAX ERRORS


data Error
    = Parse (Maybe R.Region) Problem
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
    | DuplicateFieldName R.Region Text
    | DuplicateValueDeclaration Text
    | DuplicateTypeDeclaration Text
    | DuplicateDefinition Text
    | UnboundTypeVarsInUnion Text [Text] [Text]
    | UnboundTypeVarsInAlias Text [Text] [Text]
    | UnusedTypeVarsInAlias Text [Text] [Text]
    | MessyTypeVarsInAlias Text [Text] [Text] [Text]



-- PARSE ERRORS


data ParseError = ParseError !Int !Int !Problem


data Problem
  = Tab
  | EndOfFile_Comment
  | EndOfFile_Shader
  | EndOfFile_String
  | EndOfFile_MultiString
  | EndOfFile_Char
  | NewLineInString
  | NewLineInChar
  | BadEscape Int EscapeProblem
  | BadChar Int
  | BadNumberDot
  | BadNumberEnd
  | BadNumberExp
  | BadNumberHex
  | BadNumberZero
  | BadShader Text
  | BadUnderscore Char
  | BadOp BadOp ContextStack
  | Theories ContextStack [Theory]


data EscapeProblem
  = UnknownEscape
  | UnicodeSyntax
  | UnicodeRange
  | UnicodeLength Int Text


data BadOp = HasType | Equals | Arrow | Pipe | Dot


data Theory
  = Keyword Text
  | Symbol Text
  | Expecting Next
  | LowVar
  | CapVar
  | InfixOp
  | Digit
  | BadSpace
  deriving (Eq, Ord)


data Next
  = Decl
  | Expr
  | AfterOpExpr Text
  | ElseBranch
  | Arg
  | Pattern
  | Type
  | Listing
  | Exposing
  deriving (Eq, Ord)


type ContextStack = [(Context, R.Position)]


data Context
  = ExprIf
  | ExprLet
  | ExprFunc
  | ExprCase
  | ExprList
  | ExprTuple
  | ExprRecord
  ----------------
  | Definition Text
  | Annotation Text
  ----------------
  | TypeTuple
  | TypeRecord
  ----------------
  | PatternList
  | PatternTuple
  | PatternRecord
  ----------------
  | Module
  | Import
  | TypeUnion
  | TypeAlias
  | Infix
  | Port
  deriving (Eq, Ord, Show)



-- TO REPORT


toReport :: RenderType.Localizer -> Error -> Report.Report
toReport _localizer err =
  case err of
    Parse subRegion problem ->
        problemToReport subRegion problem

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
              \ an `effect module` you forgot to use the `effect` keyword. In that\
              \ case, change `module` to `effect module` and you should be headed in\
              \ the right direction!"
          )

    SettingsOnPortModule ->
        Report.report
          "BAD MODULE DECLARATION"
          Nothing
          "A port module can expose values, but not have settings like this."
          ( reflowParagraph $
              "If you want a port module, just remove this stuff. If you want to create\
              \ a custom effect module instead (which is rare) change `port module`\
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
              "All ports must be defined in a `port module`. You should probably have only\
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


makeParseReport :: Maybe R.Region -> Text -> Help.Doc -> Report.Report
makeParseReport subRegion pre post =
  Report.report "PARSE ERROR" subRegion pre post


problemToReport :: Maybe R.Region -> Problem -> Report.Report
problemToReport subRegion problem =
  let parseReport = makeParseReport subRegion in
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

    EndOfFile_Char ->
      parseReport
        "I got to the end of the file while parsing a character."
        (reflowParagraph $
          "Characters look like 'x' with single quotes on each end.\
          \ So the closing single quote seems to be missing."
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

    NewLineInChar ->
      parseReport
        "This character is missing the closing quote."
        ( reflowParagraph $
            "Elm characters must start and end with a single quote, like 'a' and 'b'"
        )

    BadEscape _ escapeProblem ->
      uncurry parseReport (escapeProblemToReport escapeProblem)

    BadChar _ ->
      parseReport
        "Ran into a bad use of single quotes."
        ( Help.stack
            [ reflowParagraph $
                "If you want to create a string, switch to double quotes:"
            , Help.indent 4 $
                dullyellow (text "'this'")
                <> text " => "
                <> green (text "\"this\"")
            , reflowParagraph $
                "Unlike JavaScript, Elm distinguishes between strings like \"hello\"\
                \ and individual characters like 'A' and '3'. If you really do want\
                \ a character though, something went wrong and I did not find the\
                \ closing single quote."
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

    BadUnderscore char ->
      parseReport
        ("An underscore cannot be immediately followed by `" <> Text.singleton char <> "`.")
        ( reflowParagraph $
            "Variable names cannot start with an underscore in Elm. You can\
            \ use an underscore as a \"wildcard\" that matches anything\
            \ though. So maybe you forgot a space after the underscore?"
        )

    BadOp op stack ->
      case op of
        HasType ->
          badOp subRegion stack "A" "\"has type\" operator"
            "type annotations and record types"
            "Maybe you want :: instead? Or maybe something is indented too much?"

        Equals ->
          badEquals subRegion stack

        Arrow ->
          if isCaseRelated stack then
            parseReport
              "I ran into a stray arrow while parsing this `case` expression."
              ( reflowParagraph $
                  "All branches in a `case` must be indented the exact\
                  \ same amount, so the patterns are vertically\
                  \ aligned. Maybe this branch is indented too much?"
              )

          else
            badOp subRegion stack "An" "arrow"
              "cases expressions and anonymous functions"
              "Maybe you want > or >= instead?"

        Pipe ->
          badOp subRegion stack "A" "vertical bar"
            "type declarations"
            "Maybe you want || instead?"

        Dot ->
          parseReport
            "I was not expecting this dot."
            ( reflowParagraph $
                "Dots are for record access and decimal points, so\
                \ they cannot float around on their own. Maybe\
                \ there is some extra whitespace?"
            )

    Theories stack allTheories ->
      let
        starter =
          "Something went wrong while parsing "
          <> contextToText "your code" "" stack
          <> "."
      in
        parseReport starter $
          case Set.toList (Set.fromList allTheories) of
            [] ->
              Help.stack
                [ reflowParagraph $
                    "I do not have any suggestions though!"
                , reflowParagraph $
                    "Can you get it down to a <http://sscce.org> and share it at\
                    \ <https://github.com/elm-lang/error-message-catalog/issues>?\
                    \ That way we can figure out how to give better advice!"
                ]

            [theory] ->
              reflowParagraph $
                "I was expecting to see "
                <> addPeriod (theoryToText stack theory)

            theories ->
              Help.vcat $
                [ text "I was expecting:"
                , text ""
                ]
                ++ map (bullet . theoryToText stack) theories


escapeProblemToReport :: EscapeProblem -> ( Text, Help.Doc )
escapeProblemToReport escapeProblem =
  case escapeProblem of
    UnknownEscape ->
      ( "A backslash starts an escaped character, but I do not recognize this one:"
      , Help.stack
          [ "Maybe there is some typo?"
          , Help.toSimpleHint "Valid escape characters include:"
          , Help.indent 4 $ Help.vcat $
              [ "\\n"
              , "\\t"
              , "\\\""
              , "\\\'"
              , "\\\\"
              , "\\u{03BB}"
              ]
          , reflowParagraph $
              "The last one lets encode ANY character by its Unicode code\
              \ point, so there are no other escape characters in Elm."
          ]
      )

    UnicodeSyntax ->
      ( "I ran into an invalid Unicode escape character:"
      , Help.stack
          [ "Here are some examples of valid Unicode escape characters:"
          , Help.indent 4 $ Help.vcat $
              [ "\\u{0041}"
              , "\\u{03BB}"
              , "\\u{6728}"
              , "\\u{1F60A}"
              ]
          , reflowParagraph $
              "Notice that the code point is always surrounded by curly\
              \ braces. They are required!"
          ]
      )

    UnicodeRange ->
      ( "This is not a real Unicode code point:"
      , "All valid Unicode code points are between 0 and 10FFFF."
      )

    UnicodeLength numDigits badCode ->
      let
        moreOrFewer =
          if numDigits < 4 then "more" else "fewer"

        goodCode =
          Text.justifyRight 4 '0' $ Text.dropWhile (== '0') badCode

        suggestion =
          [ "Try"
          , "using"
          , dullyellow $ text $ "\\u{" <> goodCode <> "}"
          , "instead."
          ]
      in
      ( "This Unicode code point needs " <> moreOrFewer <> " digits:"
      , hsep suggestion
      )



-- HELPERS


bullet :: Text -> Help.Doc
bullet point =
  Help.hang 4 ("  - " <> reflowParagraph point)


addPeriod :: Text -> Text
addPeriod msg =
  if Text.last msg `elem` ['`', ')', '.', '!', '?'] then
    msg
  else
    msg <> "."


badOp :: Maybe R.Region -> ContextStack -> Text -> Text -> Text -> Text -> Report.Report
badOp subRegion stack article opName setting hint =
  let
    pre =
      "I was not expecting this " <> opName
      <> contextToText "" " while parsing " stack <> "."
  in
    makeParseReport subRegion pre $
      reflowParagraph $
        article <> " " <> opName <> " should only appear in "
        <> setting <> ". " <> hint


badEquals :: Maybe R.Region -> ContextStack -> Report.Report
badEquals subRegion stack =
  let
    pre =
      "I was not expecting this equals sign"
      <> contextToText "" " while parsing " stack <> "."
  in
    makeParseReport subRegion pre (badEqualsHelp stack)


badEqualsHelp :: ContextStack -> Help.Doc
badEqualsHelp stack =
  case stack of
    [] ->
      reflowParagraph $
        "Maybe you want == instead? Or maybe something is indented too much?"

    (ExprRecord, _) : _ ->
      reflowParagraph $
        "Records look like { x = 3, y = 4 } with the equals sign right\
        \ after the field name. Maybe you forgot a comma?"

    (Definition _, _) : rest ->
      reflowParagraph $
        "Maybe this is supposed to be a separate definition? If so, it\
        \ is indented too far. "
        <>
        if any ((==) ExprLet . fst) rest then
          "All definitions in a `let` expression must be vertically aligned."
        else
          "Spaces are not allowed before top-level definitions."

    _ : rest ->
      badEqualsHelp rest


contextToText :: Text -> Text -> ContextStack -> Text
contextToText defaultText prefixText stack =
  case stack of
    [] ->
      defaultText

    (context, _) : rest ->
      let anchor = getAnchor rest in
      prefixText <>
      case context of
        ExprIf -> "an `if`" <> anchor
        ExprLet -> "a `let`" <> anchor
        ExprFunc -> "an anonymous function" <> anchor
        ExprCase -> "a `case`" <> anchor
        ExprList -> "a list" <> anchor
        ExprTuple -> "an expression (in parentheses)" <> anchor
        ExprRecord -> "a record" <> anchor
        Definition name -> name <> "'s definition"
        Annotation name -> name <> "'s type annotation"
        TypeTuple -> "a type (in parentheses)" <> anchor
        TypeRecord -> "a record type" <> anchor
        PatternList -> "a list pattern" <> anchor
        PatternTuple -> "a pattern (in parentheses)" <> anchor
        PatternRecord -> "a record pattern" <> anchor
        Module -> "a module declaration"
        Import -> "an import"
        TypeUnion -> "a union type"
        TypeAlias -> "a type alias"
        Infix -> "an infix declaration"
        Port -> "a port declaration"


getAnchor :: ContextStack -> Text
getAnchor stack =
  case stack of
    [] ->
      ""

    (context, _) : rest ->
      case context of
        Definition name ->
          " in " <> name <> "'s definition"

        Annotation name ->
          " in " <> name <> "'s type annotation"

        _ ->
          getAnchor rest


isCaseRelated :: ContextStack -> Bool
isCaseRelated stack =
  case stack of
    [] ->
      False

    (context, _) : rest ->
      context == ExprCase || isCaseRelated rest


theoryToText :: ContextStack -> Theory -> Text
theoryToText context theory =
  case theory of
    Keyword keyword ->
      "the `" <> keyword <> "` keyword"

    Symbol symbol ->
      case symbol of
        "=" -> equalsTheory context
        "->" -> "an arrow (->) followed by an expression"
        ":" -> "the \"has type\" symbol (:) followed by a type"
        "," -> "a comma"
        "|" -> barTheory context
        "::" -> "the cons operator (::) followed by more list elements"
        "." -> "a dot (.)"
        "-" -> "a minus sign (-)"
        "_" -> "an underscore"
        "(" -> "a left paren, for grouping or starting tuples"
        ")" -> "a closing paren"
        "[" -> "a left square bracket, for starting lists"
        "]" -> "a right square bracket, to end a list"
        "{" -> "a left curly brace, for starting records"
        "}" -> "a right curly brace, to end a record"
        "{-|" -> "a doc comment, like {-| example -}"
        _ -> "the (" <> symbol <> ") symbol"

    LowVar ->
      "a lower-case variable, like `x` or `user`"

    CapVar ->
      "an upper-case variable, like `Maybe` or `Just`"

    InfixOp ->
      "an infix operator, like (+) or (==)"

    Digit ->
      "a digit from 0 to 9"

    BadSpace ->
      badSpace context

    Expecting next ->
      case next of
        Decl -> "a declaration, like `x = 5` or `type alias Model = { ... }`"
        Expr -> "an expression, like x or 42"
        AfterOpExpr op -> "an expression after that (" <> op <> ") operator, like x or 42"
        ElseBranch -> "an `else` branch. An `if` must handle both possibilities."
        Arg -> "an argument, like `name` or `total`"
        Pattern -> "a pattern, like `name` or (Just x)"
        Type -> "a type, like Int or (List String)"
        Listing -> "a list of exposed values and types, like (..) or (x,y,z)"
        Exposing -> "something like `exposing (..)`"


equalsTheory :: ContextStack -> Text
equalsTheory stack =
  case stack of
    [] ->
      "an equals sign (=)"

    (context, _) : rest ->
      case context of
        ExprRecord -> "an equals sign (=) followed by an expression"
        Definition name -> "an equals sign (=) followed by " <> name <> "'s definition"
        TypeUnion -> "an equals sign (=) followed by the first union type constructor"
        TypeAlias -> "an equals sign (=) followed by the aliased type"
        _ -> equalsTheory rest


barTheory :: ContextStack -> Text
barTheory stack =
  case stack of
    [] ->
      "a vertical bar (|)"

    (context, _) : rest ->
      case context of
        ExprRecord -> "a vertical bar (|) followed by the record fields you want to update"
        TypeRecord -> "a vertical bar (|) followed by some record field types"
        TypeUnion -> "a vertical bar (|) followed by more union type constructors"
        _ -> barTheory rest


badSpace :: ContextStack -> Text
badSpace stack =
  case stack of
    [] ->
      "more indentation? I was not done with that last thing yet."

    (context, _) : rest ->
      case context of
        ExprIf -> "the end of that `if`" <> badSpaceExprEnd rest
        ExprLet -> "the end of that `let`" <> badSpaceExprEnd rest
        ExprFunc -> badSpace rest
        ExprCase -> "more of that `case`" <> badSpaceExprEnd rest
        ExprList -> "the end of that list" <> badSpaceExprEnd rest
        ExprTuple -> "a closing paren" <> badSpaceExprEnd rest
        ExprRecord -> "the end of that record" <> badSpaceExprEnd rest
        Definition name -> "the rest of " <> name <> "'s definition" <> badSpaceExprEnd stack
        Annotation name -> "the rest of " <> name <> "'s type annotation" <> badSpaceEnd
        TypeTuple -> "a closing paren" <> badSpaceEnd
        TypeRecord -> "the end of that record" <> badSpaceEnd
        PatternList -> "the end of that list" <> badSpaceEnd
        PatternTuple -> "a closing paren" <> badSpaceEnd
        PatternRecord -> "the end of that record" <> badSpaceEnd
        Module -> "something like `module Main exposing (..)`"
        Import -> "something like `import Html exposing (..)`"
        TypeUnion -> "more of that union type" <> badSpaceEnd
        TypeAlias -> "more of that type alias" <> badSpaceEnd
        Infix -> "more of that infix declaration" <> badSpaceEnd
        Port -> "more of that port declaration" <> badSpaceEnd


badSpaceEnd :: Text
badSpaceEnd =
  ". Maybe you forgot some code? Or you need more indentation?"


badSpaceExprEnd :: ContextStack -> Text
badSpaceExprEnd stack =
  case stack of
    [] ->
      badSpaceEnd

    (Definition name, R.Position _ column) : _ ->
      let
        ending =
          if column <= 1 then
            "to be indented?"
          else
            "more indentation? (Try " <> i2t (column + 1) <> "+ spaces.)"
      in
        ". Maybe you forgot some code? Or maybe the body of `" <> name
        <> "` needs " <> ending

    _ : rest ->
      badSpaceExprEnd rest
