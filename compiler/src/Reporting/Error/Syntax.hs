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


import Data.Monoid ((<>))
import qualified Data.Set as Set
import qualified Data.Text as Text

import qualified Elm.Name as N
import qualified Reporting.Doc as D
import qualified Reporting.Region as R
import qualified Reporting.Render.Code as Code
import qualified Reporting.Report as Report



-- ALL SYNTAX ERRORS


data Error
    = CommentOnNothing R.Region
    | UnexpectedPort R.Region N.Name
    | TypeWithBadDefinition R.Region N.Name N.Name
    | TypeWithoutDefinition R.Region N.Name
    | Parse R.Region (Maybe R.Region) Problem



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
  | BadNumberDot Int
  | BadNumberEnd
  | BadNumberExp
  | BadNumberHex
  | BadNumberZero
  | FloatInPattern
  | BadShader Text.Text
  | BadUnderscore Int
  | BadOp BadOp ContextStack
  | Theories ContextStack [Theory]


data EscapeProblem
  = UnknownEscape
  | UnicodeSyntax
  | UnicodeRange
  | UnicodeLength Int String


data BadOp = HasType | Equals | Arrow | Pipe | Dot


data Theory
  = Expecting Next
  | Keyword String
  | Symbol String
  | LowVar
  | CapVar
  | InfixOp
  | Digit
  | BadSpace
  deriving (Eq, Ord)


data Next
  = Decl
  | Expr
  | AfterOpExpr N.Name
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
  | Definition N.Name
  | Annotation N.Name
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
  deriving (Eq, Ord)



-- TO REPORT


toReport :: Code.Source -> Error -> Report.Report
toReport source err =
  case err of
    CommentOnNothing region ->
      Report.Report "STRAY COMMENT" region [] $
        Report.toCodeSnippet source region Nothing
          (
            "This documentation comment is not followed by anything."
          ,
            D.reflow $
              "All documentation comments need to be right above the declaration they\
              \ describe. Maybe some code got deleted or commented out by accident? Or\
              \ maybe this comment is here by accident?"
          )

    UnexpectedPort region name ->
      Report.Report "BAD PORT" region [] $
        Report.toCodeSnippet source region Nothing
          (
            D.reflow $
              "You are declaring port `" <> N.toString name <> "` in a normal module."
          ,
            D.stack
              [ "It needs to be in a `port` module."
              , D.link "Hint"
                  "Ports are not a traditional FFI for calling JS functions directly. They need a different mindset! Read"
                  "ports"
                  "to learn how to use ports effectively."
              ]
          )

    TypeWithBadDefinition region annName defName ->
      Report.Report "ANNOTATION MISMATCH" region [] $
        Report.toCodeSnippet source region Nothing
          (
            D.reflow $
              "I see a `" <> N.toString annName
              <> "` annotation, but it is followed by a `"
              <> N.toString defName <> "` definition."
          ,
            D.fillSep
              ["The","annotation","and","definition","names","must","match!"
              ,"Is","there","a","typo","between"
              , D.dullyellow (D.fromName annName)
              ,"and"
              , D.dullyellow (D.fromName defName) <> "?"
              ]
          )

    TypeWithoutDefinition region name ->
      Report.Report "MISSING DEFINITION" region [] $
        Report.toCodeSnippet source region Nothing
          (
            D.reflow $
              "There is a type annotation for `" <> N.toString name
              <> "` but there is no corresponding definition!"
          ,
            "Directly below the type annotation, put a definition like:\n\n"
            <> "    " <> D.fromName name <> " = 42"
          )

    Parse region subRegion problem ->
      Report.Report "PARSE ERROR" (maybe region id subRegion) [] $
        Report.toCodeSnippet source region subRegion $
          problemToDocs problem



-- PARSE ERROR TO DOCS


problemToDocs :: Problem -> (D.Doc, D.Doc)
problemToDocs problem =
  case problem of
    Tab ->
      (
        "I ran into a tab, but tabs are not allowed in Elm files."
      ,
        "Replace the tab with spaces."
      )

    EndOfFile_Comment ->
      (
        "I got to the end of the file while parsing a multi-line comment."
      ,
        D.stack
          [ D.reflow $
              "Multi-line comments look like {- comment -}, and it looks like\
              \ you are missing the closing marker."
          , D.toSimpleHint $
              "Nested multi-line comments like {- this {- and this -} -} are allowed.\
              \ The opening and closing markers must be balanced though, just\
              \ like parentheses in normal code. Maybe that is the problem?"
          ]
      )

    EndOfFile_Shader ->
      (
        "I got to the end of the file while parsing a GLSL block."
      ,
        D.reflow $
          "A shader should be defined in a block like this: [glsl| ... |]"
      )

    EndOfFile_String ->
      (
        "I got to the end of the file while parsing a string."
      ,
        D.reflow $
          "Strings look like \"this\" with double quotes on each end.\
          \ Is the closing double quote missing in your code?"
      )

    EndOfFile_MultiString ->
      (
        "I got to the end of the file while parsing a multi-line string."
      ,
        D.reflow $
          "Multi-line strings look like \"\"\"this\"\"\" with three double quotes on each\
          \ end. Is the closing triple quote missing in your code?"
      )

    EndOfFile_Char ->
      (
        "I got to the end of the file while parsing a character."
      ,
        D.reflow $
          "Characters look like 'c' with single quotes on each end.\
          \ Is the closing single quote missing in your code?"
      )

    NewLineInString ->
      (
        "This string is missing the closing quote."
      ,
        D.stack
          [ D.reflow $
              "Elm strings like \"this\" cannot contain newlines."
          , D.toSimpleHint $
              "For strings that CAN contain newlines, say \"\"\"this\"\"\" for Elm’s\
              \ multi-line string syntax. It allows unescaped newlines and double quotes."
          ]
      )

    NewLineInChar ->
      (
        "This character is missing the closing quote."
      ,
        "Elm characters must start and end with a single quote. Valid examples\n\
        \ include 'a', 'b', '\n', 'ø', and '\\u{00F8}' for unicode code points."
      )

    BadEscape _ escapeProblem ->
      case escapeProblem of
        UnknownEscape ->
          (
            "Backslashes always start escaped characters, but I do not recognize this one:"
          , D.stack
              [ "Maybe there is some typo?"
              , D.toSimpleHint "Valid escape characters include:"
              , D.indent 4 $ D.vcat $
                  [ "\\n"
                  , "\\r"
                  , "\\t"
                  , "\\\""
                  , "\\\'"
                  , "\\\\"
                  , "\\u{03BB}"
                  ]
              , D.reflow $
                  "The last one lets encode ANY character by its Unicode code\
                  \ point, so use that for anything outside the ordinary six."
              ]
          )

        UnicodeSyntax ->
          (
            "I ran into an invalid Unicode escape character:"
          ,
            D.stack
              [ "Here are some examples of valid Unicode escape characters:"
              , D.indent 4 $ D.vcat $
                  [ "\\u{0041}"
                  , "\\u{03BB}"
                  , "\\u{6728}"
                  , "\\u{1F60A}"
                  ]
              , D.reflow $
                  "Notice that the code point is always surrounded by curly\
                  \ braces. They are required!"
              ]
          )

        UnicodeRange ->
          (
            "This is not a real Unicode code point:"
          ,
            "All valid Unicode code points are between 0 and 10FFFF."
          )

        UnicodeLength numDigits badCode ->
          if numDigits < 4 then
            (
              "Every Unicode code point needs at least four digits:"
            ,
              let
                goodCode = replicate (4 - numDigits) '0' ++ badCode
                escape = "\\u{" <> D.fromString goodCode <> "}"
              in
              D.hsep [ "Try", D.dullyellow escape, "instead?" ]
            )

          else
            (
              "This Unicode code point has too many digits:"
            ,
              D.fillSep
                ["Valid","code","points","are","between"
                , D.dullyellow "\\u{0000}", "and", D.dullyellow "\\u{10FFFF}"
                ,"so","it","must","have","between","four","and","six","digits."
                ]
            )

    BadChar _ ->
      (
        "Ran into a bad use of single quotes."
      ,
        D.stack
          [ D.reflow $
              "If you want to create a string, switch to double quotes:"
          , D.indent 4 $
              D.dullyellow "'this'" <> " => " <> D.green "\"this\""
          , D.toSimpleHint $
              "Unlike JavaScript, Elm distinguishes between strings like \"hello\"\
              \ and individual characters like 'A' and '3'. If you really do want\
              \ a character though, something went wrong and I did not find the\
              \ closing single quote."
          ]
      )

    BadNumberDot numberBeforeDot ->
      (
        "Numbers cannot end with a decimal points."
      ,
        let
          number =
            D.fromString (show numberBeforeDot)
        in
        "Saying " <> D.green number <> " or " <> D.green (number <> ".0") <> " will work though!"
      )

    BadNumberEnd ->
      (
        "Numbers cannot have letters or underscores in them."
      ,
        D.reflow $
          "Maybe a space is missing between a number and a variable?"
      )

    BadNumberExp ->
      (
        "If you put the letter E in a number, it should followed by more digits."
      ,
        D.reflow $
          "If you want to say 1000, you can also say 1e3.\
          \ You cannot just end it with an E though!"
      )

    BadNumberHex ->
      (
        "I see the start of a hex number, but not the end."
      ,
        D.reflow $
          "A hex number looks like 0x123ABC, where the 0x is followed by hexidecimal\
          \ digits. Valid hexidecimal digits include: 0123456789abcdefABCDEF"
      )

    BadNumberZero ->
      (
        "Normal numbers cannot start with zeros. Take the zeros off the front."
      ,
        D.reflow $
          "Only numbers like 0x0040 or 0.25 can start with a zero."
      )

    FloatInPattern ->
      (
        "I cannot pattern match with floating point numbers:"
      ,
        D.reflow $
          "Equality on floats can be unreliable, so you usually want to check that they\
          \ are nearby with some sort of (abs (actual - expected) < 0.001) check."
      )

    BadShader msg ->
      (
        "I ran into a problem while parsing this GLSL block."
      ,
        D.reflow (Text.unpack msg)
      )

    BadUnderscore _ ->
      (
        "A variable name cannot start with an underscore:"
      ,
        D.reflow $
          "You can (1) use a wildcard like _ to ignore the value or you can (2) use\
          \ a name that starts with a letter to access the value later. Pick one!"
      )

    BadOp op stack ->
      case op of
        HasType ->
          badOp stack "A" "\"has type\" operator"
            "type annotations and record types"
            "Maybe you want :: instead? Or maybe something is indented too much?"

        Equals ->
          (
            D.reflow $
              "I was not expecting this equals sign"
              <> contextToString " here" " while parsing " stack <> "."
          ,
            toBadEqualsHint stack
          )

        Arrow ->
          if isCaseRelated stack then
            (
              "I ran into a stray arrow while parsing this `case` expression."
            ,
              D.reflow $
                "All branches in a `case` must be indented the exact\
                \ same amount, so the patterns are vertically\
                \ aligned. Maybe this branch is indented too much?"
            )

          else
            badOp stack "An" "arrow"
              "cases expressions and anonymous functions"
              "Maybe you want > or >= instead?"

        Pipe ->
          badOp stack "A" "vertical bar"
            "type declarations"
            "Maybe you want || instead?"

        Dot ->
          (
            "I was not expecting this dot."
          ,
            D.reflow $
              "Dots are for record access and decimal points, so\
              \ they cannot float around on their own. Maybe\
              \ there is some extra whitespace?"
          )

    Theories stack allTheories ->
      (
        D.reflow $
          "Something went wrong while parsing " <> contextToString "your code" "" stack <> "."
      ,
        case Set.toList (Set.fromList allTheories) of
          [] ->
            D.stack
              [ D.reflow $
                  "I do not have any suggestions though!"
              , D.reflow $
                  "Can you get it down to a <http://sscce.org> and share it at\
                  \ <https://github.com/elm/error-message-catalog/issues>?\
                  \ That way we can figure out how to give better advice!"
              ]

          [theory] ->
            D.reflow $
              "I was expecting to see "
              <> addPeriod (theoryToString stack theory)

          theories ->
            D.vcat $
              [ "I was expecting:"
              , ""
              ]
              ++ map (bullet . theoryToString stack) theories
      )



-- BAD OP HELPERS


badOp :: ContextStack -> String -> String -> String -> String -> ( D.Doc, D.Doc )
badOp stack article opName setting hint =
  (
    D.reflow $
      "I was not expecting this " <> opName
      <> contextToString " here" " while parsing " stack <> "."
  ,
    D.reflow $
      article <> " " <> opName <> " should only appear in "
      <> setting <> ". " <> hint
  )


toBadEqualsHint :: ContextStack -> D.Doc
toBadEqualsHint stack =
  case stack of
    [] ->
      D.reflow $
        "Maybe you want == instead? Or maybe something is indented too much?"

    (ExprRecord, _) : _ ->
      D.reflow $
        "Records look like { x = 3, y = 4 } with the equals sign right\
        \ after the field name. Maybe you forgot a comma?"

    (Definition _, _) : rest ->
      D.reflow $
        "Maybe this is supposed to be a separate definition? If so, it\
        \ is indented too far. "
        <>
        if any ((==) ExprLet . fst) rest then
          "All definitions in a `let` expression must be vertically aligned."
        else
          "Spaces are not allowed before top-level definitions."

    _ : rest ->
      toBadEqualsHint rest



isCaseRelated :: ContextStack -> Bool
isCaseRelated stack =
  case stack of
    [] ->
      False

    (context, _) : rest ->
      context == ExprCase || isCaseRelated rest



-- CONTEXT


contextToString :: String -> String -> ContextStack -> String
contextToString defaultString prefixString stack =
  case stack of
    [] ->
      defaultString

    (context, _) : rest ->
      let anchor = getAnchor rest in
      prefixString <>
      case context of
        ExprIf -> "an `if` expression" <> anchor
        ExprLet -> "a `let` expression" <> anchor
        ExprFunc -> "an anonymous function" <> anchor
        ExprCase -> "a `case` expression" <> anchor
        ExprList -> "a list" <> anchor
        ExprTuple -> "an expression (in parentheses)" <> anchor
        ExprRecord -> "a record" <> anchor
        Definition name -> N.toString name <> "'s definition"
        Annotation name -> N.toString name <> "'s type annotation"
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


getAnchor :: ContextStack -> String
getAnchor stack =
  case stack of
    [] ->
      ""

    (context, _) : rest ->
      case context of
        Definition name ->
          " in " <> N.toString name <> "'s definition"

        Annotation name ->
          " in " <> N.toString name <> "'s type annotation"

        _ ->
          getAnchor rest



-- THEORY HELPERS


bullet :: String -> D.Doc
bullet point =
  D.hang 4 ("  - " <> D.fillSep (map D.fromString (words point)))


addPeriod :: String -> String
addPeriod msg =
  if last msg `elem` ['`', ')', '.', '!', '?'] then
    msg
  else
    msg <> "."


theoryToString :: ContextStack -> Theory -> String
theoryToString context theory =
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
        "{-|" -> "a doc comment, like {-| this -}"
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
        AfterOpExpr op -> "an expression after that (" <> N.toString op <> ") operator, like x or 42"
        ElseBranch -> "an `else` branch. An `if` must handle both possibilities."
        Arg -> "an argument, like `name` or `total`"
        Pattern -> "a pattern, like `name` or (Just x)"
        Type -> "a type, like Int or (List String)"
        Listing -> "a list of exposed values and types, like (..) or (x,y,z)"
        Exposing -> "something like `exposing (..)`"


equalsTheory :: ContextStack -> String
equalsTheory stack =
  case stack of
    [] ->
      "an equals sign (=)"

    (context, _) : rest ->
      case context of
        ExprRecord -> "an equals sign (=) followed by an expression"
        Definition name -> "an equals sign (=) followed by " <> N.toString name <> "'s definition"
        TypeUnion -> "an equals sign (=) followed by the first union type constructor"
        TypeAlias -> "an equals sign (=) followed by the aliased type"
        _ -> equalsTheory rest


barTheory :: ContextStack -> String
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


badSpace :: ContextStack -> String
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
        Definition name -> "the rest of " <> N.toString name <> "'s definition" <> badSpaceExprEnd stack
        Annotation name -> "the rest of " <> N.toString name <> "'s type annotation" <> badSpaceEnd
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


badSpaceEnd :: String
badSpaceEnd =
  ". Maybe you forgot some code? Or you need more indentation?"


badSpaceExprEnd :: ContextStack -> String
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
            "more indentation? (Try " <> show (column + 1) <> "+ spaces.)"
      in
        ". Maybe you forgot some code? Or maybe the body of `"
        <> N.toString name
        <> "` needs " <> ending

    _ : rest ->
      badSpaceExprEnd rest
