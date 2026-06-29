{-# LANGUAGE BangPatterns, ExtendedLiterals, MagicHash, OverloadedStrings, UnboxedTuples #-}
module Reporting.Error.Syntax
  ( Error(..)
  , toReport
  --
  , Module(..)
  , Exposing(..)
  --
  , Decl(..)
  , DeclType(..)
  , TypeAlias(..)
  , CustomType(..)
  , DeclDef(..)
  , Port(..)
  --
  , Expr(..)
  , Record(..)
  , Tuple(..)
  , List(..)
  , Func(..)
  , Case(..)
  , If(..)
  , Let(..)
  , Def(..)
  , Destruct(..)
  --
  , Pattern(..)
  , PRecord(..)
  , PTuple(..)
  , PList(..)
  --
  , Type(..)
  , TRecord(..)
  , TTuple(..)
  --
  , Char(..)
  , String(..)
  , Escape(..)
  , Number(..)
  --
  , Space(..)
  , toSpaceReport
  )
  where


import Prelude hiding (Char, String)
import qualified Data.Char as Char
import qualified Data.Name as Name
import GHC.Prim
import GHC.Word (Word32(..), Word64(..))
import Numeric (showHex)

import qualified Elm.ModuleName as ModuleName
import Parse.Primitives (Cursor, Indent, slide, isLineStart, distanceToIndent)
import Parse.Symbol (BadOperator(..))
import qualified Reporting.Annotation as A
import qualified Reporting.Doc as D
import qualified Reporting.Report as Report
import qualified Reporting.Render.Code as Code



-- ALL SYNTAX ERRORS


data Error
  = ModuleNameUnspecified ModuleName.Raw
  | ModuleNameMismatch ModuleName.Raw (A.Located ModuleName.Raw)
  | UnexpectedPort A.Region
  | NoPorts A.Region
  | NoPortsInPackage (A.Located Name.Name)
  | NoPortModulesInPackage A.Region
  | NoEffectsOutsideKernel A.Region
  | ParseError Module



-- MODULE


data Module
  = ModuleSpace Space Cursor
  | ModuleBadEnd Cursor
  --
  | ModuleProblem Cursor
  | ModuleName Cursor
  | ModuleExposing Exposing Cursor
  --
  | PortModuleProblem Cursor
  | PortModuleName Cursor
  | PortModuleExposing Exposing Cursor
  --
  | Effect Cursor
  --
  | FreshLine Cursor
  --
  | ImportStart Cursor
  | ImportName Cursor
  | ImportAs Cursor
  | ImportAlias Cursor
  | ImportExposing Cursor
  | ImportExposingList Exposing Cursor
  | ImportEnd Cursor -- different based on col=1 or if greater
  --
  | ImportIndentName Cursor
  | ImportIndentAlias Cursor
  | ImportIndentExposingList Cursor
  --
  | Infix Cursor
  --
  | Declarations Decl Cursor


data Exposing
  = ExposingSpace Space Cursor
  | ExposingStart Cursor
  | ExposingValue Cursor
  | ExposingOperator Cursor
  | ExposingOperatorReserved BadOperator Cursor
  | ExposingOperatorRightParen Cursor
  | ExposingTypePrivacy Cursor
  | ExposingEnd Cursor
  --
  | ExposingIndentEnd Cursor
  | ExposingIndentValue Cursor



-- DECLARATIONS


data Decl
  = DeclStart Cursor
  | DeclSpace Space Cursor
  --
  | Port Port Cursor
  | DeclType DeclType Cursor
  | DeclDef Name.Name DeclDef Cursor
  --
  | DeclFreshLineAfterDocComment Cursor


data DeclDef
  = DeclDefSpace Space Cursor
  | DeclDefEquals Cursor
  | DeclDefType Type Cursor
  | DeclDefArg Pattern Cursor
  | DeclDefBody Expr Cursor
  | DeclDefNameRepeat Cursor
  | DeclDefNameMatch Name.Name Cursor
  --
  | DeclDefIndentType Cursor
  | DeclDefIndentEquals Cursor
  | DeclDefIndentBody Cursor


data Port
  = PortSpace Space Cursor
  | PortName Cursor
  | PortColon Cursor
  | PortType Type Cursor
  | PortIndentName Cursor
  | PortIndentColon Cursor
  | PortIndentType Cursor



-- TYPE DECLARATIONS


data DeclType
  = DT_Space Space Cursor
  | DT_Name Cursor
  | DT_Alias TypeAlias Cursor
  | DT_Union CustomType Cursor
  --
  | DT_IndentName Cursor


data TypeAlias
  = AliasSpace Space Cursor
  | AliasName Cursor
  | AliasEquals Cursor
  | AliasBody Type Cursor
  --
  | AliasIndentEquals Cursor
  | AliasIndentBody Cursor


data CustomType
  = CT_Space Space Cursor
  | CT_Name Cursor
  | CT_Equals Cursor
  | CT_Bar Cursor
  | CT_Variant Cursor
  | CT_VariantArg Type Cursor
  --
  | CT_IndentEquals Cursor
  | CT_IndentBar Cursor
  | CT_IndentAfterBar Cursor
  | CT_IndentAfterEquals Cursor



-- EXPRESSIONS


data Expr
  = Let Let Cursor
  | Case Case Cursor
  | If If Cursor
  | List List Cursor
  | Record Record Cursor
  | Tuple Tuple Cursor
  | Func Func Cursor
  --
  | Dot Cursor
  | Access Cursor
  | OperatorRight Name.Name Cursor
  | OperatorReserved BadOperator Cursor
  --
  | Start Cursor
  | Char Char Cursor
  | String String Cursor
  | Number Number Cursor
  | Space Space Cursor
  | ShaderEndless Cursor
  | ShaderNotUtf8 Cursor
  | ShaderProblem [Char.Char] Cursor
  | IndentOperatorRight Name.Name Cursor


data Record
  = RecordOpen Cursor
  | RecordEnd Cursor
  | RecordField Cursor
  | RecordEquals Cursor
  | RecordExpr Expr Cursor
  | RecordSpace Space Cursor
  --
  | RecordIndentOpen Cursor
  | RecordIndentEnd Cursor
  | RecordIndentField Cursor
  | RecordIndentEquals Cursor
  | RecordIndentExpr Cursor


data Tuple
  = TupleExpr Expr Cursor
  | TupleSpace Space Cursor
  | TupleEnd Cursor
  | TupleOperatorClose Cursor
  | TupleOperatorReserved BadOperator Cursor
  --
  | TupleIndentExpr1 Cursor
  | TupleIndentExprN Cursor
  | TupleIndentEnd Cursor


data List
  = ListSpace Space Cursor
  | ListOpen Cursor
  | ListExpr Expr Cursor
  | ListEnd Cursor
  --
  | ListIndentOpen Cursor
  | ListIndentEnd Cursor
  | ListIndentExpr Cursor


data Func
  = FuncSpace Space Cursor
  | FuncArg Pattern Cursor
  | FuncBody Expr Cursor
  | FuncArrow Cursor
  --
  | FuncIndentArg Cursor
  | FuncIndentArrow Cursor
  | FuncIndentBody Cursor


data Case
  = CaseSpace Space Cursor
  | CaseOf Cursor
  | CasePattern Pattern Cursor
  | CaseArrow Cursor
  | CaseExpr Expr Cursor
  | CaseBranch Expr Cursor
  --
  | CaseIndentOf Cursor
  | CaseIndentExpr Cursor
  | CaseIndentPattern Cursor
  | CaseIndentArrow Cursor
  | CaseIndentBranch Cursor
  | CasePatternAlignment Indent Cursor


data If
  = IfSpace Space Cursor
  | IfThen Cursor
  | IfElse Cursor
  | IfElseBranchStart Cursor
  --
  | IfCondition Expr Cursor
  | IfThenBranch Expr Cursor
  | IfElseBranch Expr Cursor
  --
  | IfIndentCondition Cursor
  | IfIndentThen Cursor
  | IfIndentThenBranch Cursor
  | IfIndentElseBranch Cursor
  | IfIndentElse Cursor


data Let
  = LetSpace Space Cursor
  | LetIn Cursor
  | LetDefAlignment Indent Cursor
  | LetDefName Cursor
  | LetDef Name.Name Def Cursor
  | LetDestruct Destruct Cursor
  | LetBody Expr Cursor
  | LetIndentDef Cursor
  | LetIndentIn Cursor
  | LetIndentBody Cursor


data Def
  = DefSpace Space Cursor
  | DefType Type Cursor
  | DefNameRepeat Cursor
  | DefNameMatch Name.Name Cursor
  | DefArg Pattern Cursor
  | DefEquals Cursor
  | DefBody Expr Cursor
  | DefIndentEquals Cursor
  | DefIndentType Cursor
  | DefIndentBody Cursor
  | DefAlignment Indent Cursor


data Destruct
  = DestructSpace Space Cursor
  | DestructPattern Pattern Cursor
  | DestructEquals Cursor
  | DestructBody Expr Cursor
  | DestructIndentEquals Cursor
  | DestructIndentBody Cursor



-- PATTERNS


data Pattern
  = PRecord PRecord Cursor
  | PTuple PTuple Cursor
  | PList PList Cursor
  --
  | PStart Cursor
  | PChar Char Cursor
  | PString String Cursor
  | PNumber Number Cursor
  | PFloat Word64# Cursor
  | PAlias Cursor
  | PWildcardNotVar Name.Name Word64# Cursor
  | PSpace Space Cursor
  --
  | PIndentStart Cursor
  | PIndentAlias Cursor


data PRecord
  = PRecordOpen Cursor
  | PRecordEnd Cursor
  | PRecordField Cursor
  | PRecordSpace Space Cursor
  --
  | PRecordIndentOpen Cursor
  | PRecordIndentEnd Cursor
  | PRecordIndentField Cursor


data PTuple
  = PTupleOpen Cursor
  | PTupleEnd Cursor
  | PTupleExpr Pattern Cursor
  | PTupleSpace Space Cursor
  --
  | PTupleIndentEnd Cursor
  | PTupleIndentExpr1 Cursor
  | PTupleIndentExprN Cursor


data PList
  = PListOpen Cursor
  | PListEnd Cursor
  | PListExpr Pattern Cursor
  | PListSpace Space Cursor
  --
  | PListIndentOpen Cursor
  | PListIndentEnd Cursor
  | PListIndentExpr Cursor



-- TYPES


data Type
  = TRecord TRecord Cursor
  | TTuple TTuple Cursor
  --
  | TStart Cursor
  | TSpace Space Cursor
  --
  | TIndentStart Cursor


data TRecord
  = TRecordOpen Cursor
  | TRecordEnd Cursor
  --
  | TRecordField Cursor
  | TRecordColon Cursor
  | TRecordType Type Cursor
  --
  | TRecordSpace Space Cursor
  --
  | TRecordIndentOpen Cursor
  | TRecordIndentField Cursor
  | TRecordIndentColon Cursor
  | TRecordIndentType Cursor
  | TRecordIndentEnd Cursor


data TTuple
  = TTupleOpen Cursor
  | TTupleEnd Cursor
  | TTupleType Type Cursor
  | TTupleSpace Space Cursor
  --
  | TTupleIndentType1 Cursor
  | TTupleIndentTypeN Cursor
  | TTupleIndentEnd Cursor



-- LITERALS


data Char
  = CharNotUtf8
  | CharEndless
  | CharEscape Escape
  | CharNotString Word64#


data String
  = StringNotUtf8
  | StringEndless_Single
  | StringEndless_Multi
  | StringEscape Escape


data Escape
  = EscapeUnknown
  | BadUnicodeFormat Word64#
  | BadUnicodeCode Word64#
  | BadUnicodeLength Word64# Int Integer


data Number
  = NumberEnd
  | NumberDot Integer
  | NumberHexDigit
  | NumberNoLeadingZero



-- MISC


data Space
  = HasTab
  | HasNonUtf8
  | EndlessMultiComment



-- TO REPORT


toReport :: Code.Source -> Error -> Report.Report
toReport source err =
  case err of
    ModuleNameUnspecified name ->
      let
        region = A.zero
      in
      Report.Report "MODULE NAME MISSING" region [] $
        D.stack
          [ D.reflow $
              "I need the module name to be declared at the top of this file, like this:"
          , D.indent 4 $ D.fillSep $
              [ D.cyan "module", D.fromName name, D.cyan "exposing", "(..)" ]
          , D.reflow $
              "Try adding that as the first line of your file!"
          , D.toSimpleNote $
              "It is best to replace (..) with an explicit list of types and\
              \ functions you want to expose. When you know a value is only used\
              \ within this module, you can refactor without worrying about uses\
              \ elsewhere. Limiting exposed values can also speed up compilation\
              \ because I can skip a bunch of work if I see that the exposed API\
              \ has not changed."
          ]

    ModuleNameMismatch expectedName (A.At region actualName) ->
      Report.Report "MODULE NAME MISMATCH" region [ModuleName.toChars expectedName] $
        Code.toSnippet source region Nothing
          (
            "It looks like this module name is out of sync:"
          ,
            D.stack
              [ D.reflow $
                  "I need it to match the file path, so I was expecting to see `"
                  ++ ModuleName.toChars expectedName
                  ++ "` here. Make the following change, and you should be all set!"
              , D.indent 4 $
                  D.dullyellow (D.fromName actualName) <> " -> " <> D.green (D.fromName expectedName)
              , D.toSimpleNote $
                  "I require that module names correspond to file paths. This makes it much\
                  \ easier to explore unfamiliar codebases! So if you want to keep the current\
                  \ module name, try renaming the file instead."
              ]
          )

    UnexpectedPort region ->
      Report.Report "UNEXPECTED PORTS" region [] $
        Code.toSnippet source region Nothing
          (
            D.reflow $
              "You are declaring ports in a normal module."
          ,
            D.stack
              [ D.fillSep
                  ["Switch","this","to","say",D.cyan "port module","instead,"
                  ,"marking","that","this","module","contains","port","declarations."
                  ]
              , D.link "Note"
                  "Ports are not a traditional FFI for calling JS functions directly. They need a different mindset! Read"
                  "ports"
                  "to learn the syntax and how to use it effectively."
              ]
          )

    NoPorts region ->
      Report.Report "NO PORTS" region [] $
        Code.toSnippet source region Nothing
          (
            D.reflow $
              "This module does not declare any ports, but it says it will:"
          ,
            D.fillSep
              ["Switch","this","to",D.cyan "module"
              ,"and","you","should","be","all","set!"
              ]
          )

    NoPortsInPackage (A.At region _) ->
      Report.Report "PACKAGES CANNOT HAVE PORTS" region [] $
        Code.toSnippet source region Nothing
          (
            D.reflow $
              "Packages cannot declare any ports, so I am getting stuck here:"
          ,
            D.stack
              [ D.reflow $
                  "Remove this port declaration."
              , noteForPortsInPackage
              ]
          )

    NoPortModulesInPackage region ->
      Report.Report "PACKAGES CANNOT HAVE PORTS" region [] $
        Code.toSnippet source region Nothing
          (
            D.reflow $
              "Packages cannot declare any ports, so I am getting stuck here:"
          ,
            D.stack
              [ D.fillSep $
                  ["Remove","the",D.cyan "port","keyword","and","I"
                  ,"should","be","able","to","continue."
                  ]
              , noteForPortsInPackage
              ]
          )

    NoEffectsOutsideKernel region ->
      Report.Report "INVALID EFFECT MODULE" region [] $
        Code.toSnippet source region Nothing
          (
            D.reflow $
              "It is not possible to declare an `effect module` outside the @elm organization,\
              \ so I am getting stuck here:"
          ,
            D.stack
              [ D.reflow $
                  "Switch to a normal module declaration."
              , D.toSimpleNote $
                  "Effect modules are designed to allow certain core functionality to be\
                  \ defined separately from the compiler. So the @elm organization has access to\
                  \ this so that certain changes, extensions, and fixes can be introduced without\
                  \ needing to release new Elm binaries. For example, we want to make it possible\
                  \ to test effects, but this may require changes to the design of effect modules.\
                  \ By only having them defined in the @elm organization, that kind of design work\
                  \ can proceed much more smoothly."
              ]
          )

    ParseError modul ->
      toParseErrorReport source modul


noteForPortsInPackage :: D.Doc
noteForPortsInPackage =
  D.stack
    [ D.toSimpleNote $
        "One of the major goals of the package ecosystem is to be completely written\
        \ in Elm. This means when you install an Elm package, you can be sure you are safe\
        \ from security issues on install and that you are not going to get any runtime\
        \ exceptions coming from your new dependency. This design also sets the ecosystem\
        \ up to target other platforms more easily (like mobile phones, WebAssembly, etc.)\
        \ since no community code explicitly depends on JavaScript even existing."
    , D.reflow $
        "Given that overall goal, allowing ports in packages would lead to some pretty\
        \ surprising behavior. If ports were allowed in packages, you could install a\
        \ package but not realize that it brings in an indirect dependency that defines a\
        \ port. Now you have a program that does not work and the fix is to realize that\
        \ some JavaScript needs to be added for a dependency you did not even know about.\
        \ That would be extremely frustrating! \"So why not allow the package author to\
        \ include the necessary JS code as well?\" Now we are back in conflict with our\
        \ overall goal to keep all community packages free from runtime exceptions."
    ]


toParseErrorReport :: Code.Source -> Module -> Report.Report
toParseErrorReport source modul =
  case modul of
    ModuleSpace space cur ->
      toSpaceReport source space cur

    ModuleBadEnd cur ->
      if isLineStart cur
      then toDeclStartReport source cur
      else toWeirdEndReport source cur

    ModuleProblem cur ->
      let
        region = toRegion cur
      in
      Report.Report "UNFINISHED MODULE DECLARATION" region [] $
        Code.toSnippet source region Nothing
          (
            D.reflow $
              "I am parsing an `module` declaration, but I got stuck here:"
          ,
            D.stack
              [ D.reflow $
                  "Here are some examples of valid `module` declarations:"
              , D.indent 4 $ D.vcat $
                  [ D.fillSep [D.cyan "module","Main",D.cyan "exposing","(..)"]
                  , D.fillSep [D.cyan "module","Dict",D.cyan "exposing","(Dict, empty, get)"]
                  ]
              , D.reflow $
                  "I generally recommend using an explicit exposing list. I can skip compiling a bunch\
                  \ of files when the public interface of a module stays the same, so exposing fewer\
                  \ values can help improve compile times!"
              ]
          )

    ModuleName cur ->
      let
        region = toRegion cur
      in
      Report.Report "EXPECTING MODULE NAME" region [] $
        Code.toSnippet source region Nothing
          (
            D.reflow $
              "I was parsing an `module` declaration until I got stuck here:"
          ,
            D.stack
              [ D.reflow $
                  "I was expecting to see the module name next, like in these examples:"
              , D.indent 4 $ D.vcat $
                  [ D.fillSep [D.cyan "module","Dict",D.cyan "exposing","(..)"]
                  , D.fillSep [D.cyan "module","Maybe",D.cyan "exposing","(..)"]
                  , D.fillSep [D.cyan "module","Html.Attributes",D.cyan "exposing","(..)"]
                  , D.fillSep [D.cyan "module","Json.Decode",D.cyan "exposing","(..)"]
                  ]
              , D.reflow $
                  "Notice that the module names all start with capital letters. That is required!"
              ]
          )

    ModuleExposing exposing cur ->
      toExposingReport source exposing cur

    PortModuleProblem cur ->
      let
        region = toRegion cur
      in
      Report.Report "UNFINISHED PORT MODULE DECLARATION" region [] $
        Code.toSnippet source region Nothing
          (
            D.reflow $
              "I am parsing an `port module` declaration, but I got stuck here:"
          ,
            D.stack
              [ D.reflow $
                  "Here are some examples of valid `port module` declarations:"
              , D.indent 4 $ D.vcat $
                  [ D.fillSep [D.cyan "port",D.cyan "module","WebSockets",D.cyan "exposing","(send, listen, keepAlive)"]
                  , D.fillSep [D.cyan "port",D.cyan "module","Maps",D.cyan "exposing","(Location, goto)"]
                  ]
              , D.link "Note" "Read" "ports" "for more help."
              ]
          )

    PortModuleName cur ->
      let
        region = toRegion cur
      in
      Report.Report "EXPECTING MODULE NAME" region [] $
        Code.toSnippet source region Nothing
          (
            D.reflow $
              "I was parsing an `module` declaration until I got stuck here:"
          ,
            D.stack
              [ D.reflow $
                  "I was expecting to see the module name next, like in these examples:"
              , D.indent 4 $ D.vcat $
                  [ D.fillSep [D.cyan "port",D.cyan "module","WebSockets",D.cyan "exposing","(send, listen, keepAlive)"]
                  , D.fillSep [D.cyan "port",D.cyan "module","Maps",D.cyan "exposing","(Location, goto)"]
                  ]
              , D.reflow $
                  "Notice that the module names start with capital letters. That is required!"
              ]
          )

    PortModuleExposing exposing cur ->
      toExposingReport source exposing cur

    Effect cur ->
      let
        region = toRegion cur
      in
      Report.Report "BAD MODULE DECLARATION" region [] $
        Code.toSnippet source region Nothing
          (
            D.reflow $
              "I cannot parse this module declaration:"
          ,
            D.reflow $
              "This type of module is reserved for the @elm organization. It is used to\
              \ define certain effects, avoiding building them into the compiler."
          )

    FreshLine cur ->
      let
        region = toRegion cur

        toBadFirstLineReport keyword =
          Report.Report "TOO MUCH INDENTATION" region [] $
            Code.toSnippet source region Nothing
              (
                D.reflow $
                  "This `" ++ keyword ++ "` should not have any spaces before it:"
              ,
                D.reflow $
                  "Delete the spaces before `" ++ keyword ++ "` until there are none left!"
              )

      in
      case Code.whatIsNext source cur of
        Code.Keyword "module" -> toBadFirstLineReport "module"
        Code.Keyword "import" -> toBadFirstLineReport "import"
        Code.Keyword "type" -> toBadFirstLineReport "type"
        Code.Keyword "port" -> toBadFirstLineReport "port"
        _ ->
          Report.Report "SYNTAX PROBLEM" region [] $
            Code.toSnippet source region Nothing
              (
                D.reflow $
                  "I got stuck here:"
              ,
                D.stack
                  [ D.reflow $
                      "I am not sure what is going on, but I recommend starting an Elm\
                      \ file with the following lines:"
                  , D.indent 4 $ D.vcat $
                      [ D.fillSep [D.cyan "import","Html"]
                      , ""
                      , "main ="
                      , "  Html.text " <> D.dullyellow "\"Hello!\""
                      ]
                  , D.reflow $
                      "You should be able to copy those lines directly into your file. Check out the\
                      \ examples at <https://elm-lang.org/examples> for more help getting started!"
                  , D.toSimpleNote $
                      "This can also happen when something is indented too much!"
                  ]
              )

    ImportStart cur ->
      toImportReport source cur

    ImportName cur ->
      let
        region = toRegion cur
      in
      Report.Report "EXPECTING IMPORT NAME" region [] $
        Code.toSnippet source region Nothing
          (
            D.reflow $
              "I was parsing an `import` until I got stuck here:"
          ,
            D.stack
              [ D.reflow $
                  "I was expecting to see a module name next, like in these examples:"
              , D.indent 4 $ D.vcat $
                  [ D.fillSep [D.cyan "import","Dict"]
                  , D.fillSep [D.cyan "import","Maybe"]
                  , D.fillSep [D.cyan "import","Html.Attributes",D.cyan "as","A"]
                  , D.fillSep [D.cyan "import","Json.Decode",D.cyan "exposing","(..)"]
                  ]
              , D.reflow $
                  "Notice that the module names all start with capital letters. That is required!"
              , D.reflowLink "Read" "imports" "to learn more."
              ]
          )

    ImportAs cur ->
      toImportReport source cur

    ImportAlias cur ->
      let
        region = toRegion cur
      in
      Report.Report "EXPECTING IMPORT ALIAS" region [] $
        Code.toSnippet source region Nothing
          (
            D.reflow $
              "I was parsing an `import` until I got stuck here:"
          ,
            D.stack
              [ D.reflow $
                  "I was expecting to see an alias next, like in these examples:"
              , D.indent 4 $ D.vcat $
                  [ D.fillSep [D.cyan "import","Html.Attributes",D.cyan "as","Attr"]
                  , D.fillSep [D.cyan "import","WebGL.Texture",D.cyan "as","Texture"]
                  , D.fillSep [D.cyan "import","Json.Decode",D.cyan "as","D"]
                  ]
              , D.reflow $
                  "Notice that the alias always starts with a capital letter. That is required!"
              , D.reflowLink "Read" "imports" "to learn more."
              ]
          )

    ImportExposing cur ->
      toImportReport source cur

    ImportExposingList exposing cur ->
      toExposingReport source exposing cur

    ImportEnd cur ->
      toImportReport source cur

    ImportIndentName cur ->
      toImportReport source cur

    ImportIndentAlias cur ->
      toImportReport source cur

    ImportIndentExposingList cur ->
      let
        region = toRegion cur
      in
      Report.Report "UNFINISHED IMPORT" region [] $
        Code.toSnippet source region Nothing
          (
            D.reflow $
              "I was parsing an `import` until I got stuck here:"
          ,
            D.stack
              [ D.reflow $
                  "I was expecting to see the list of exposed values next. For example, here\
                  \ are two ways to expose values from the `Html` module:"
              , D.indent 4 $ D.vcat $
                  [ D.fillSep [D.cyan "import","Html",D.cyan "exposing","(..)"]
                  , D.fillSep [D.cyan "import","Html",D.cyan "exposing","(Html, div, text)"]
                  ]
              , D.reflow $
                  "I generally recommend the second style. It is more explicit, making it\
                  \ much easier to figure out where values are coming from in large projects!"
              ]
          )

    Infix cur ->
      let
        region = toRegion cur
      in
      Report.Report "BAD INFIX" region [] $
        Code.toSnippet source region Nothing
          (
            D.reflow $
              "Something went wrong in this infix operator declaration:"
          ,
            D.reflow $
              "This feature is used by the @elm organization to define the\
              \ languages built-in operators."
          )

    Declarations decl _ ->
      toDeclarationsReport source decl



-- WEIRD END


toWeirdEndReport :: Code.Source -> Cursor -> Report.Report
toWeirdEndReport source cur =
  case Code.whatIsNext source cur of
    Code.Keyword keyword ->
      let
        region = toKeywordRegion cur keyword
      in
      Report.Report "RESERVED WORD" region [] $
        Code.toSnippet source region Nothing
          (
            D.reflow $
              "I got stuck on this reserved word:"
          ,
            D.reflow $
              "The name `" ++ keyword ++ "` is reserved, so try using a different name?"
          )

    Code.Operator op ->
      let
        region = toKeywordRegion cur op
      in
      Report.Report "UNEXPECTED SYMBOL" region [] $
        Code.toSnippet source region Nothing
          (
            D.reflow $
              "I ran into an unexpected symbol:"
          ,
            D.reflow $
              "I was not expecting to see a " ++ op ++ " here. Try deleting it? Maybe\
              \ I can give a better hint from there?"
          )

    Code.Close term bracket ->
      let
        region = toRegion cur
      in
      Report.Report ("UNEXPECTED " ++ map Char.toUpper term) region [] $
        Code.toSnippet source region Nothing
          (
            D.reflow $
              "I ran into an unexpected " ++ term ++ ":"
          ,
            D.reflow $
              "This " ++ bracket : " does not match up with an earlier open " ++ term ++ ". Try deleting it?"
          )

    Code.Lower c cs ->
      let
        region = toKeywordRegion cur (c:cs)
      in
      Report.Report "UNEXPECTED NAME" region [] $
        Code.toSnippet source region Nothing
          (
            D.reflow $
              "I got stuck on this name:"
          ,
            D.reflow $
              "It is confusing me a lot! Normally I can give fairly specific hints, but\
              \ something is really tripping me up this time."
          )

    Code.Upper c cs ->
      let
        region = toKeywordRegion cur (c:cs)
      in
      Report.Report "UNEXPECTED NAME" region [] $
        Code.toSnippet source region Nothing
          (
            D.reflow $
              "I got stuck on this name:"
          ,
            D.reflow $
              "It is confusing me a lot! Normally I can give fairly specific hints, but\
              \ something is really tripping me up this time."
          )

    Code.Other maybeChar ->
      let
        region = toRegion cur
      in
      case maybeChar of
        Just ';' ->
          Report.Report "UNEXPECTED SEMICOLON" region [] $
            Code.toSnippet source region Nothing
              (
                D.reflow $
                  "I got stuck on this semicolon:"
              ,
                D.stack
                  [ D.reflow $ "Try removing it?"
                  , D.toSimpleNote $
                      "Some languages require semicolons at the end of each statement. These are\
                      \ often called C-like languages, and they usually share a lot of language design\
                      \ choices. (E.g. side-effects, for loops, etc.) Elm manages effects with commands\
                      \ and subscriptions instead, so there is no special syntax for \"statements\" and\
                      \ therefore no need to use semicolons to separate them. I think this will make\
                      \ more sense as you work through <https://guide.elm-lang.org> though!"
                  ]
              )

        Just ',' ->
          Report.Report "UNEXPECTED COMMA" region [] $
            Code.toSnippet source region Nothing
              (
                D.reflow $
                  "I got stuck on this comma:"
              ,
                D.stack
                  [ D.reflow $
                      "I do not think I am parsing a list or tuple right now. Try deleting the comma?"
                  , D.toSimpleNote $
                      "If this is supposed to be part of a list, the problem may be a bit earlier.\
                      \ Perhaps the opening [ is missing? Or perhaps some value in the list has an extra\
                      \ closing ] that is making me think the list ended earlier? The same kinds of\
                      \ things could be going wrong if this is supposed to be a tuple."
                  ]
              )

        Just '`' ->
          Report.Report "UNEXPECTED CHARACTER" region [] $
            Code.toSnippet source region Nothing
              (
                D.reflow $
                  "I got stuck on this character:"
              ,
                D.stack
                  [ D.reflow $
                      "It is not used for anything in Elm syntax. It is used for multi-line strings in\
                      \ some languages though, so if you want a string that spans multiple lines, you\
                      \ can use Elm's multi-line string syntax like this:"
                  , D.dullyellow $ D.indent 4 $ D.vcat $
                      [ "\"\"\""
                      , "# Multi-line Strings"
                      , ""
                      , "- start with triple double quotes"
                      , "- write whatever you want"
                      , "- no need to escape newlines or double quotes"
                      , "- end with triple double quotes"
                      , "\"\"\""
                      ]
                  , D.reflow $
                      "Otherwise I do not know what is going on! Try removing the character?"
                  ]
              )

        Just '$' ->
          Report.Report "UNEXPECTED SYMBOL" region [] $
            Code.toSnippet source region Nothing
              (
                D.reflow $
                  "I got stuck on this dollar sign:"
              ,
                D.reflow $
                  "It is not used for anything in Elm syntax. Are you coming from a language where\
                  \ dollar signs can be used in variable names? If so, try a name that (1) starts\
                  \ with a letter and (2) only contains letters, numbers, and underscores."
              )

        Just c | elem c ['#','@','!','%','~'] ->
          Report.Report "UNEXPECTED SYMBOL" region [] $
            Code.toSnippet source region Nothing
              (
                D.reflow $
                  "I got stuck on this symbol:"
              ,
                D.reflow $
                  "It is not used for anything in Elm syntax. Try removing it?"
              )

        _ ->
          Report.Report "SYNTAX PROBLEM" region [] $
            Code.toSnippet source region Nothing
              (
                D.reflow $
                  "I got stuck here:"
              ,
                D.reflow $
                  "Whatever I am running into is confusing me a lot! Normally I can give fairly\
                  \ specific hints, but something is really tripping me up this time."
              )



-- IMPORTS


toImportReport :: Code.Source -> Cursor -> Report.Report
toImportReport source cur =
  let
    region = toRegion cur
  in
  Report.Report "UNFINISHED IMPORT" region [] $
    Code.toSnippet source region Nothing
      (
        D.reflow $
          "I am partway through parsing an import, but I got stuck here:"
      ,
        D.stack
          [ D.reflow $
              "Here are some examples of valid `import` declarations:"
          , D.indent 4 $ D.vcat $
              [ D.fillSep [D.cyan "import","Html"]
              , D.fillSep [D.cyan "import","Html",D.cyan "as","H"]
              , D.fillSep [D.cyan "import","Html",D.cyan "as","H",D.cyan "exposing","(..)"]
              , D.fillSep [D.cyan "import","Html",D.cyan "exposing","(Html, div, text)"]
              ]
          , D.reflow $
              "You are probably trying to import a different module, but try to make it look like one of these examples!"
          , D.reflowLink "Read" "imports" "to learn more."
          ]
      )



-- EXPOSING


toExposingReport :: Code.Source -> Exposing -> Cursor -> Report.Report
toExposingReport source exposing startCur =
  case exposing of
    ExposingSpace space cur ->
      toSpaceReport source space cur

    ExposingStart cur ->
      let
        surroundings = A.Region startCur cur
        region = toRegion cur
      in
      Report.Report "PROBLEM IN EXPOSING" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I want to parse exposed values, but I am getting stuck here:"
          ,
            D.stack
              [ D.fillSep $
                  ["Exposed","values","are","always","surrounded","by","parentheses."
                  ,"So","try","adding","a",D.green "(","here?"
                  ]
              , D.toSimpleNote "Here are some valid examples of `exposing` for reference:"
              , D.indent 4 $ D.vcat $
                  [ D.fillSep [D.cyan "import","Html",D.cyan "exposing","(..)"]
                  , D.fillSep [D.cyan "import","Html",D.cyan "exposing","(Html, div, text)"]
                  ]
              , D.reflow $
                  "If you are getting tripped up, you can just expose everything for now. It should\
                  \ get easier to make an explicit exposing list as you see more examples in the wild."
              ]
          )

    ExposingValue cur ->
      case Code.whatIsNext source cur of
        Code.Keyword keyword ->
          let
            surroundings = A.Region startCur cur
            region = toKeywordRegion cur keyword
          in
          Report.Report "RESERVED WORD" region [] $
            Code.toSnippet source surroundings (Just region)
              (
                D.reflow $
                  "I got stuck on this reserved word:"
              ,
                D.reflow $
                  "It looks like you are trying to expose `" ++ keyword ++ "` but that is a reserved word. Is there a typo?"
              )

        Code.Operator op ->
          let
            surroundings = A.Region startCur cur
            region = toKeywordRegion cur op
          in
          Report.Report "UNEXPECTED SYMBOL" region [] $
            Code.toSnippet source surroundings (Just region)
              (
                D.reflow $
                  "I got stuck on this symbol:"
              ,
                D.stack
                  [ D.reflow $
                      "If you are trying to expose an operator, add parentheses around it like this:"
                  , D.indent 4 $ D.dullyellow (D.fromChars op) <> " -> " <> D.green ("(" <> D.fromChars op <> ")")
                  ]
              )

        _ ->
          let
            surroundings = A.Region startCur cur
            region = toRegion cur
          in
          Report.Report "PROBLEM IN EXPOSING" region [] $
            Code.toSnippet source surroundings (Just region)
              (
                D.reflow $
                  "I got stuck while parsing these exposed values:"
              ,
                D.stack
                  [ D.reflow $
                      "I do not have an exact recommendation, so here are some valid examples\
                      \ of `exposing` for reference:"
                  , D.indent 4 $ D.vcat $
                      [ D.fillSep [D.cyan "import","Html",D.cyan "exposing","(..)"]
                      , D.fillSep [D.cyan "import","Basics",D.cyan "exposing","(Int, Float, Bool(..), (+), not, sqrt)"]
                      ]
                  , D.reflow $
                      "These examples show how to expose types, variants, operators, and functions. Everything\
                      \ should be some permutation of these examples, just with different names."
                  ]
              )

    ExposingOperator cur ->
      let
        surroundings = A.Region startCur cur
        region = toRegion cur
      in
      Report.Report "PROBLEM IN EXPOSING" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I just saw an open parenthesis, so I was expecting an operator next:"
          ,
            D.fillSep $
              ["It","is","possible","to","expose","operators,","so","I","was","expecting"
              ,"to","see","something","like",D.dullyellow "(+)","or",D.dullyellow "(|=)"
              ,"or",D.dullyellow "(||)","after","I","saw","that","open","parenthesis."
              ]
          )

    ExposingOperatorReserved op cur ->
      let
        surroundings = A.Region startCur cur
        region = toRegion cur
      in
      Report.Report "RESERVED SYMBOL" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I cannot expose this as an operator:"
          ,
            case op of
              BadDot -> D.reflow "Try getting rid of this entry? Maybe I can give you a better hint after that?"
              BadPipe -> D.fillSep ["Maybe","you","want",D.dullyellow "(||)","instead?"]
              BadArrow -> D.reflow "Try getting rid of this entry? Maybe I can give you a better hint after that?"
              BadEquals -> D.fillSep ["Maybe","you","want",D.dullyellow "(==)","instead?"]
              BadHasType -> D.fillSep ["Maybe","you","want",D.dullyellow "(::)","instead?"]
          )

    ExposingOperatorRightParen cur ->
      let
        surroundings = A.Region startCur cur
        region = toRegion cur
      in
      Report.Report "PROBLEM IN EXPOSING" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "It looks like you are exposing an operator, but I got stuck here:"
          ,
            D.fillSep $
              ["I","was","expecting","to","see","the","closing","parenthesis","immediately"
              ,"after","the","operator.","Try","adding","a",D.green ")","right","here?"
              ]
          )

    ExposingEnd cur ->
      let
        surroundings = A.Region startCur cur
        region = toRegion cur
      in
      Report.Report "UNFINISHED EXPOSING" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I was partway through parsing exposed values, but I got stuck here:"
          ,
            D.reflow $
              "Maybe there is a comma missing before this?"
          )

    ExposingTypePrivacy cur ->
      let
        surroundings = A.Region startCur cur
        region = toRegion cur
      in
      Report.Report "PROBLEM EXPOSING CUSTOM TYPE VARIANTS" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "It looks like you are trying to expose the variants of a custom type:"
          ,
            D.stack
              [ D.fillSep $
                  ["You","need","to","write","something","like"
                  ,D.dullyellow "Status(..)","or",D.dullyellow "Entity(..)"
                  ,"though.","It","is","all","or","nothing,","otherwise","`case`"
                  ,"expressions","could","miss","a","variant","and","crash!"
                  ]
              , D.toSimpleNote $
                  "It is often best to keep the variants hidden! If someone pattern matches on\
                  \ the variants, it is a MAJOR change if any new variants are added. Suddenly\
                  \ their `case` expressions do not cover all variants! So if you do not need\
                  \ people to pattern match, keep the variants hidden and expose functions to\
                  \ construct values of this type. This way you can add new variants as a MINOR change!"
              ]
          )

    ExposingIndentEnd cur ->
      let
        surroundings = A.Region startCur cur
        region = toRegion cur
      in
      Report.Report "UNFINISHED EXPOSING" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I was partway through parsing exposed values, but I got stuck here:"
          ,
            D.stack
              [ D.fillSep $
                  ["I","was","expecting","a","closing","parenthesis."
                  ,"Try","adding","a",D.green ")","right","here?"
                  ]
              , D.toSimpleNote $
                  "I can get confused when there is not enough indentation, so if you already\
                  \ have a closing parenthesis, it probably just needs some spaces in front of it."
              ]
          )

    ExposingIndentValue cur ->
      let
        surroundings = A.Region startCur cur
        region = toRegion cur
      in
      Report.Report "UNFINISHED EXPOSING" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I was partway through parsing exposed values, but I got stuck here:"
          ,
            D.reflow $
              "I was expecting another value to expose."
          )



-- SPACES


toSpaceReport :: Code.Source -> Space -> Cursor -> Report.Report
toSpaceReport source space cur =
  case space of
    HasTab ->
      let
        region = toRegion cur
      in
      Report.Report "NO TABS" region [] $
        Code.toSnippet source region Nothing
          (
            D.reflow $
              "I ran into a tab, but tabs are not allowed in Elm files."
          ,
            D.reflow $
              "Replace the tab with spaces."
          )

    HasNonUtf8 ->
      let
        region = toRegion cur
      in
      Report.Report "UNEXPECTED ENCODING" region [] $
        Code.toSnippet source region Nothing
          (
            D.reflow $
              "Elm files use UTF-8 character encoding, but I ran into something outside of that format:"
          ,
            D.reflow $
              "Is there a way to convert this to a valid UTF-8 character?"
          )

    EndlessMultiComment ->
      let
        region = toWiderRegion cur 2#Word64
      in
      Report.Report "ENDLESS COMMENT" region [] $
        Code.toSnippet source region Nothing
          (
            D.reflow $
              "I cannot find the end of this multi-line comment:"
          ,
            D.stack -- "{-"
              [ D.reflow "Add a -} somewhere after this to end the comment."
              , D.toSimpleHint
                  "Multi-line comments can be nested in Elm, so {- {- -} -} is a comment\
                  \ that happens to contain another comment. Like parentheses and curly braces,\
                  \ the start and end markers must always be balanced. Maybe that is the problem?"
              ]
          )



-- DECLARATIONS


toRegion :: Cursor -> A.Region
toRegion cur =
  A.Region cur cur


toWiderRegion :: Cursor -> Word64# -> A.Region
toWiderRegion cur extra =
  A.Region cur (slide cur extra)


toKeywordRegion :: Cursor -> [Char.Char] -> A.Region
toKeywordRegion cur keyword =
    A.Region cur (slide cur len)
  where
    !(W64# len) = fromIntegral (length keyword)


toDeclarationsReport :: Code.Source -> Decl -> Report.Report
toDeclarationsReport source decl =
  case decl of
    DeclStart cur ->
      toDeclStartReport source cur

    DeclSpace space cur ->
      toSpaceReport source space cur

    Port port_ cur ->
      toPortReport source port_ cur

    DeclType declType cur ->
      toDeclTypeReport source declType cur

    DeclDef name declDef cur ->
      toDeclDefReport source name declDef cur

    DeclFreshLineAfterDocComment cur ->
      let
        region = toRegion cur
      in
      Report.Report "EXPECTING DECLARATION" region [] $
        Code.toSnippet source region Nothing
          (
            D.reflow $
              "I just saw a doc comment, but then I got stuck here:"
          ,
            D.reflow $
              "I was expecting to see the corresponding declaration next, starting on a fresh\
              \ line with no indentation."
          )


toDeclStartReport :: Code.Source -> Cursor -> Report.Report
toDeclStartReport source cur =
  case Code.whatIsNext source cur of
    Code.Close term bracket ->
      let
        region = toRegion cur
      in
      Report.Report ("STRAY " ++ map Char.toUpper term) region [] $
        Code.toSnippet source region Nothing
          (
            D.reflow $
              "I was not expecting to see a " ++ term ++ " here:"
          , D.reflow $
              "This " ++ bracket : " does not match up with an earlier open " ++ term ++ ". Try deleting it?"
          )

    Code.Keyword keyword ->
      let
        region = toKeywordRegion cur keyword
      in
      Report.Report "RESERVED WORD" region [] $
        Code.toSnippet source region Nothing
          (
            D.reflow $
              "I was not expecting to run into the `" ++ keyword ++ "` keyword here:"
          ,
            case keyword of
              "import" ->
                D.reflow $
                  "It is reserved for declaring imports at the top of your module. If you want\
                  \ another import, try moving it up top with the other imports. If you want to\
                  \ define a value or function, try changing the name to something else!"

              "case" ->
                D.stack
                  [ D.reflow $
                      "It is reserved for writing `case` expressions. Try using a different name?"
                  , D.toSimpleNote $
                      "If you are trying to write a `case` expression, it needs to be part of a\
                      \ definition. So you could write something like this instead:"
                  , D.indent 4 $ D.vcat $
                      [ D.indent 0 $ D.fillSep ["getWidth","maybeWidth","="]
                      , D.indent 2 $ D.fillSep [D.cyan "case","maybeWidth",D.cyan "of"]
                      , D.indent 4 $ D.fillSep [D.blue "Just","width","->"]
                      , D.indent 6 $ D.fillSep ["width","+",D.dullyellow "200"]
                      , ""
                      , D.indent 4 $ D.fillSep [D.blue "Nothing","->"]
                      , D.indent 6 $ D.fillSep [D.dullyellow "400"]
                      ]
                  , D.reflow $
                      "This defines a `getWidth` function that you can use elsewhere in your program."
                  ]

              "if" ->
                D.stack
                  [ D.reflow $
                      "It is reserved for writing `if` expressions. Try using a different name?"
                  , D.toSimpleNote $
                      "If you are trying to write an `if` expression, it needs to be part of a\
                      \ definition. So you could write something like this instead:"
                  , D.indent 4 $ D.vcat $
                      [ "greet name ="
                      , D.fillSep $
                          [" "
                          ,D.cyan "if","name","==",D.dullyellow "\"Abraham Lincoln\""
                          ,D.cyan "then",D.dullyellow "\"Greetings Mr. President.\""
                          ,D.cyan "else",D.dullyellow "\"Hey!\""
                          ]
                      ]
                  , D.reflow $
                      "This defines a `greet` function that you can use elsewhere in your program."
                  ]

              _ ->
                D.reflow $
                  "It is a reserved word. Try changing the name to something else?"
          )

    Code.Upper c cs ->
      let
        region = toRegion cur
      in
      Report.Report "UNEXPECTED CAPITAL LETTER" region [] $
        Code.toSnippet source region Nothing
          (
            D.reflow $
              "Declarations always start with a lower-case letter, so I am getting stuck here:"
          ,
            D.stack
              [ D.fillSep $
                  ["Try","a","name","like"
                  ,D.green (D.fromChars (Char.toLower c : cs))
                  ,"instead?"
                  ]
              , D.toSimpleNote $
                  "Here are a couple valid declarations for reference:"
              , D.indent 4 $ D.vcat $
                  [ "greet : String -> String"
                  , "greet name ="
                  , "  " <> D.dullyellow "\"Hello \"" <> " ++ name ++ " <> D.dullyellow "\"!\""
                  , ""
                  , D.cyan "type" <> " User = Anonymous | LoggedIn String"
                  ]
              , D.reflow $
                  "Notice that they always start with a lower-case letter. Capitalization matters!"
              ]
          )

    Code.Other (Just char) | elem char ['(', '{', '[', '+', '-', '*', '/', '^', '&', '|', '"', '\'', '!', '@', '#', '$', '%'] ->
      let
        region = toRegion cur
      in
      Report.Report "UNEXPECTED SYMBOL" region [] $
        Code.toSnippet source region Nothing
          (
            D.reflow $
              "I am getting stuck because this line starts with the " ++ [char] ++ " symbol:"
          ,
            D.stack
              [ D.reflow $
                  "When a line has no spaces at the beginning, I expect it to be a declaration like one of these:"
              , D.indent 4 $ D.vcat $
                  [ "greet : String -> String"
                  , "greet name ="
                  , "  " <> D.dullyellow "\"Hello \"" <> " ++ name ++ " <> D.dullyellow "\"!\""
                  , ""
                  , D.cyan "type" <> " User = Anonymous | LoggedIn String"
                  ]
              , D.reflow $
                  "If this is not supposed to be a declaration, try adding some spaces before it?"
              ]
          )

    _ ->
      let
        region = toRegion cur
      in
      Report.Report "WEIRD DECLARATION" region [] $
        Code.toSnippet source region Nothing
          (
            D.reflow $
              "I am trying to parse a declaration, but I am getting stuck here:"
          ,
            D.stack
              [ D.reflow $
                  "When a line has no spaces at the beginning, I expect it to be a declaration like one of these:"
              , D.indent 4 $ D.vcat $
                  [ "greet : String -> String"
                  , "greet name ="
                  , "  " <> D.dullyellow "\"Hello \"" <> " ++ name ++ " <> D.dullyellow "\"!\""
                  , ""
                  , D.cyan "type" <> " User = Anonymous | LoggedIn String"
                  ]
              , D.reflow $
                  "Try to make your declaration look like one of those? Or if this is not\
                  \ supposed to be a declaration, try adding some spaces before it?"
              ]
          )


-- PORT


toPortReport :: Code.Source -> Port -> Cursor -> Report.Report
toPortReport source port_ startCur =
  case port_ of
    PortSpace space cur ->
      toSpaceReport source space cur

    PortName cur ->
      case Code.whatIsNext source cur of
        Code.Keyword keyword ->
          let
            surroundings = A.Region startCur cur
            region = toKeywordRegion cur keyword
          in
          Report.Report "RESERVED WORD" region [] $
            Code.toSnippet source surroundings (Just region)
              (
                D.reflow $
                  "I cannot handle ports with names like this:"
              ,
                D.reflow $
                  "You are trying to make a port named `" ++ keyword
                  ++ "` but that is a reserved word. Try using some other name?"
              )

        _ ->
          let
            surroundings = A.Region startCur cur
            region = toRegion cur
          in
          Report.Report "PORT PROBLEM" region [] $
            Code.toSnippet source surroundings (Just region)
              (
                D.reflow $
                  "I just saw the start of a `port` declaration, but then I got stuck here:"
              ,
                D.stack
                  [ D.fillSep
                      ["I","was","expecting","to","see","a","name","like"
                      ,D.dullyellow "send","or",D.dullyellow "receive","next."
                      ,"Something","that","starts","with","a","lower-case","letter."
                      ]
                  , portNote
                  ]
              )

    PortColon cur ->
      let
        surroundings = A.Region startCur cur
        region = toRegion cur
      in
      Report.Report "PORT PROBLEM" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I just saw the start of a `port` declaration, but then I got stuck here:"
          ,
            D.stack
              [ D.reflow $
                  "I was expecting to see a colon next. And then a type that tells me\
                  \ what type of values are going to flow through."
              , portNote
              ]
          )

    PortType tipe cur ->
      toTypeReport source TC_Port tipe cur

    PortIndentName cur ->
      let
        surroundings = A.Region startCur cur
        region = toRegion cur
      in
      Report.Report "UNFINISHED PORT" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I just saw the start of a `port` declaration, but then I got stuck here:"
          ,
            D.stack
              [ D.fillSep
                  ["I","was","expecting","to","see","a","name","like"
                  ,D.dullyellow "send","or",D.dullyellow "receive","next."
                  ,"Something","that","starts","with","a","lower-case","letter."
                  ]
              , portNote
              ]
          )

    PortIndentColon cur ->
      let
        surroundings = A.Region startCur cur
        region = toRegion cur
      in
      Report.Report "UNFINISHED PORT" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I just saw the start of a `port` declaration, but then I got stuck here:"
          ,
            D.stack
              [ D.reflow $
                  "I was expecting to see a colon next. And then a type that tells me\
                  \ what type of values are going to flow through."
              , portNote
              ]
          )

    PortIndentType cur ->
      let
        surroundings = A.Region startCur cur
        region = toRegion cur
      in
      Report.Report "UNFINISHED PORT" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I just saw the start of a `port` declaration, but then I got stuck here:"
          ,
            D.stack
              [ D.reflow $
                  "I was expecting to see a type next. Here are examples of outgoing and\
                  \ incoming ports for reference:"
              , D.indent 4 $ D.vcat $
                  [ D.fillSep [D.cyan "port","send",":","String -> Cmd msg"]
                  , D.fillSep [D.cyan "port","receive",":","(String -> msg) -> Sub msg"]
                  ]
              , D.reflow $
                  "The first line defines a `send` port so you can send strings out to JavaScript.\
                  \ Maybe you send them on a WebSocket or put them into IndexedDB. The second line\
                  \ defines a `receive` port so you can receive strings from JavaScript. Maybe you\
                  \ get receive messages when new WebSocket messages come in or when an entry in\
                  \ IndexedDB changes for some external reason."
              ]
          )


portNote :: D.Doc
portNote =
  D.stack
    [ D.toSimpleNote $
        "Here are some example `port` declarations for reference:"
    , D.indent 4 $ D.vcat $
        [ D.fillSep [D.cyan "port","send",":","String -> Cmd msg"]
        , D.fillSep [D.cyan "port","receive",":","(String -> msg) -> Sub msg"]
        ]
    , D.reflow $
        "The first line defines a `send` port so you can send strings out to JavaScript.\
        \ Maybe you send them on a WebSocket or put them into IndexedDB. The second line\
        \ defines a `receive` port so you can receive strings from JavaScript. Maybe you\
        \ get receive messages when new WebSocket messages come in or when the IndexedDB\
        \ is changed for some external reason."
    ]



-- DECL TYPE


toDeclTypeReport :: Code.Source -> DeclType -> Cursor -> Report.Report
toDeclTypeReport source declType startCur =
  case declType of
    DT_Space space cur ->
      toSpaceReport source space cur

    DT_Name cur ->
      let
        surroundings = A.Region startCur cur
        region = toRegion cur
      in
      Report.Report "EXPECTING TYPE NAME" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I think I am parsing a type declaration, but I got stuck here:"
          ,
            D.stack
              [ D.fillSep $
                  ["I","was","expecting","a","name","like",D.dullyellow "Status","or",D.dullyellow "Style"
                  ,"next.","Just","make","sure","it","is","a","name","that","starts","with","a","capital","letter!"
                  ]
              , customTypeNote
              ]
          )

    DT_Alias typeAlias cur ->
      toTypeAliasReport source typeAlias cur

    DT_Union customType cur ->
      toCustomTypeReport source customType cur

    DT_IndentName cur ->
      let
        surroundings = A.Region startCur cur
        region = toRegion cur
      in
      Report.Report "EXPECTING TYPE NAME" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I think I am parsing a type declaration, but I got stuck here:"
          ,
            D.stack
              [ D.fillSep $
                  ["I","was","expecting","a","name","like",D.dullyellow "Status","or",D.dullyellow "Style"
                  ,"next.","Just","make","sure","it","is","a","name","that","starts","with","a","capital","letter!"
                  ]
              , customTypeNote
              ]
          )


toTypeAliasReport :: Code.Source -> TypeAlias -> Cursor -> Report.Report
toTypeAliasReport source typeAlias startCur =
  case typeAlias of
    AliasSpace space cur ->
      toSpaceReport source space cur

    AliasName cur ->
      let
        surroundings = A.Region startCur cur
        region = toRegion cur
      in
      Report.Report "EXPECTING TYPE ALIAS NAME" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I am partway through parsing a type alias, but I got stuck here:"
          ,
            D.stack
              [ D.fillSep $
                  ["I","was","expecting","a","name","like",D.dullyellow "Person","or",D.dullyellow "Point"
                  ,"next.","Just","make","sure","it","is","a","name","that","starts","with","a","capital","letter!"
                  ]
              , typeAliasNote
              ]
          )

    AliasEquals cur ->
      case Code.whatIsNext source cur of
        Code.Keyword keyword ->
          let
            surroundings = A.Region startCur cur
            region = toKeywordRegion cur keyword
          in
          Report.Report "RESERVED WORD" region [] $
            Code.toSnippet source surroundings (Just region)
              (
                D.reflow $
                  "I ran into a reserved word unexpectedly while parsing this type alias:"
              ,
                D.stack
                  [ D.reflow $
                      "It looks like you are trying use `" ++ keyword
                      ++ "` as a type variable, but it is a reserved word. Try using a different name?"
                  , typeAliasNote
                  ]
              )

        _ ->
          let
            surroundings = A.Region startCur cur
            region = toRegion cur
          in
          Report.Report "PROBLEM IN TYPE ALIAS" region [] $
            Code.toSnippet source surroundings (Just region)
              (
                D.reflow $
                  "I am partway through parsing a type alias, but I got stuck here:"
              ,
                D.stack
                  [ D.reflow $
                      "I was expecting to see a type variable or an equals sign next."
                  , typeAliasNote
                  ]
              )

    AliasBody tipe cur ->
      toTypeReport source TC_TypeAlias tipe cur

    AliasIndentEquals cur ->
      let
        surroundings = A.Region startCur cur
        region = toRegion cur
      in
      Report.Report "UNFINISHED TYPE ALIAS" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I am partway through parsing a type alias, but I got stuck here:"
          ,
            D.stack
              [ D.reflow $
                  "I was expecting to see a type variable or an equals sign next."
              , typeAliasNote
              ]
          )

    AliasIndentBody cur ->
      let
        surroundings = A.Region startCur cur
        region = toRegion cur
      in
      Report.Report "UNFINISHED TYPE ALIAS" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I am partway through parsing a type alias, but I got stuck here:"
          ,
            D.stack
              [ D.fillSep $
                  ["I","was","expecting","to","see","a","type","next.","Something","as","simple"
                  ,"as",D.dullyellow "Int","or",D.dullyellow "Float","would","work!"
                  ]
              , typeAliasNote
              ]
          )


typeAliasNote :: D.Doc
typeAliasNote =
  D.stack
    [ D.toSimpleNote $
        "Here is an example of a valid `type alias` for reference:"
    , D.vcat $
        [ D.indent 4 $ D.fillSep [D.cyan "type",D.cyan "alias","Person","="]
        , D.indent 6 $ D.vcat $
             ["{ name : String"
             ,", age : Int"
             ,", height : Float"
             ,"}"
             ]
        ]
    , D.reflow $
        "This would let us use `Person` as a shorthand for that record type. Using this\
        \ shorthand makes type annotations much easier to read, and makes changing code\
        \ easier if you decide later that there is more to a person than age and height!"
    ]


toCustomTypeReport :: Code.Source -> CustomType -> Cursor -> Report.Report
toCustomTypeReport source customType startCur =
  case customType of
    CT_Space space cur ->
      toSpaceReport source space cur

    CT_Name cur ->
      let
        surroundings = A.Region startCur cur
        region = toRegion cur
      in
      Report.Report "EXPECTING TYPE NAME" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I think I am parsing a type declaration, but I got stuck here:"
          ,
            D.stack
              [ D.fillSep $
                  ["I","was","expecting","a","name","like",D.dullyellow "Status","or",D.dullyellow "Style"
                  ,"next.","Just","make","sure","it","is","a","name","that","starts","with","a","capital","letter!"
                  ]
              , customTypeNote
              ]
          )

    CT_Equals cur ->
      case Code.whatIsNext source cur of
        Code.Keyword keyword ->
          let
            surroundings = A.Region startCur cur
            region = toKeywordRegion cur keyword
          in
          Report.Report "RESERVED WORD" region [] $
            Code.toSnippet source surroundings (Just region)
              (
                D.reflow $
                  "I ran into a reserved word unexpectedly while parsing this custom type:"
              ,
                D.stack
                  [ D.reflow $
                      "It looks like you are trying use `" ++ keyword
                      ++ "` as a type variable, but it is a reserved word. Try using a different name?"
                  , customTypeNote
                  ]
              )

        _ ->
          let
            surroundings = A.Region startCur cur
            region = toRegion cur
          in
          Report.Report "PROBLEM IN CUSTOM TYPE" region [] $
            Code.toSnippet source surroundings (Just region)
              (
                D.reflow $
                  "I am partway through parsing a custom type, but I got stuck here:"
              ,
                D.stack
                  [ D.reflow $
                      "I was expecting to see a type variable or an equals sign next."
                  , customTypeNote
                  ]
              )

    CT_Bar cur ->
      let
        surroundings = A.Region startCur cur
        region = toRegion cur
      in
      Report.Report "PROBLEM IN CUSTOM TYPE" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I am partway through parsing a custom type, but I got stuck here:"
          ,
            D.stack
              [ D.reflow $
                  "I was expecting to see a vertical bar like | next."
              , customTypeNote
              ]
          )

    CT_Variant cur ->
      let
        surroundings = A.Region startCur cur
        region = toRegion cur
      in
      Report.Report "PROBLEM IN CUSTOM TYPE" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I am partway through parsing a custom type, but I got stuck here:"
          ,
            D.stack
              [ D.fillSep $
                  ["I","was","expecting","to","see","a","variant","name","next."
                  ,"Something","like",D.dullyellow "Success","or",D.dullyellow "Sandwich" <> "."
                  ,"Any","name","that","starts","with","a","capital","letter","really!"
                  ]
              , customTypeNote
              ]
          )

    CT_VariantArg tipe cur ->
      toTypeReport source TC_CustomType tipe cur

    CT_IndentEquals cur ->
      let
        surroundings = A.Region startCur cur
        region = toRegion cur
      in
      Report.Report "UNFINISHED CUSTOM TYPE" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I am partway through parsing a custom type, but I got stuck here:"
          ,
            D.stack
              [ D.reflow $
                  "I was expecting to see a type variable or an equals sign next."
              , customTypeNote
              ]
          )

    CT_IndentBar cur ->
      let
        surroundings = A.Region startCur cur
        region = toRegion cur
      in
      Report.Report "UNFINISHED CUSTOM TYPE" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I am partway through parsing a custom type, but I got stuck here:"
          ,
            D.stack
              [ D.reflow $
                  "I was expecting to see a vertical bar like | next."
              , customTypeNote
              ]
          )

    CT_IndentAfterBar cur ->
      let
        surroundings = A.Region startCur cur
        region = toRegion cur
      in
      Report.Report "UNFINISHED CUSTOM TYPE" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I am partway through parsing a custom type, but I got stuck here:"
          ,
            D.stack
              [ D.reflow $
                  "I just saw a vertical bar, so I was expecting to see another variant defined next."
              , customTypeNote
              ]
          )

    CT_IndentAfterEquals cur ->
      let
        surroundings = A.Region startCur cur
        region = toRegion cur
      in
      Report.Report "UNFINISHED CUSTOM TYPE" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I am partway through parsing a custom type, but I got stuck here:"
          ,
            D.stack
              [ D.reflow $
                  "I just saw an equals sign, so I was expecting to see the first variant defined next."
              , customTypeNote
              ]
          )


customTypeNote :: D.Doc
customTypeNote =
  D.stack
    [ D.toSimpleNote $
        "Here is an example of a valid `type` declaration for reference:"
    , D.vcat $
        [ D.indent 4 $ D.fillSep [D.cyan "type","Status"]
        , D.indent 6 $ D.fillSep ["=","Failure"]
        , D.indent 6 $ D.fillSep ["|","Waiting"]
        , D.indent 6 $ D.fillSep ["|","Success","String"]
        ]
    , D.reflow $
        "This defines a new `Status` type with three variants. This could be useful if\
        \ we are waiting for an HTTP request. Maybe we start with `Waiting` and then\
        \ switch to `Failure` or `Success \"message from server\"` depending on how\
        \ things go. Notice that the Success variant has some associated data, allowing\
        \ us to store a String if the request goes well!"
    ]



-- DECL DEF


toDeclDefReport :: Code.Source -> Name.Name -> DeclDef -> Cursor -> Report.Report
toDeclDefReport source name declDef startCur =
  case declDef of
    DeclDefSpace space cur ->
      toSpaceReport source space cur

    DeclDefEquals cur ->
      case Code.whatIsNext source cur of
        Code.Keyword keyword ->
          let
            surroundings = A.Region startCur cur
            region = toKeywordRegion cur keyword
          in
          Report.Report "RESERVED WORD" region [] $
            Code.toSnippet source surroundings (Just region)
              (
                D.fillSep
                  ["The","name"
                  ,"`" <> D.cyan (D.fromChars keyword) <> "`"
                  ,"is","reserved","in","Elm,","so","it","cannot"
                  ,"be","used","as","an","argument","here:"
                  ]
              ,
                D.stack
                  [ D.reflow $
                      "Try renaming it to something else."
                  , case keyword of
                      "as" ->
                        D.toFancyNote
                          ["This","keyword","is","reserved","for","pattern","matches","like"
                          ,"((x,y)",D.cyan "as","point)","where","you","want","to","name","a","tuple","and"
                          ,"the","values","it","contains."
                          ]

                      _ ->
                        D.toSimpleNote $
                          "The `" ++ keyword ++ "` keyword has a special meaning in Elm, so it can only be used in certain situations."
                  ]
              )

        Code.Operator "->" ->
          let
            surroundings = A.Region startCur cur
            region = toWiderRegion cur 2#Word64
          in
          Report.Report "MISSING COLON?" region [] $
            Code.toSnippet source surroundings (Just region)
              (
                D.reflow $
                  "I was not expecting to see an arrow here:"
              ,
                D.stack
                  [ D.fillSep
                      ["This","usually","means","a",D.green ":","is","missing","a","bit","earlier","in"
                      ,"a","type","annotation.","It","could","be","something","else","though,","so"
                      ,"here","is","a","valid","definition","for","reference:"
                      ]
                  , D.indent 4 $ D.vcat $
                      [ "greet : String -> String"
                      , "greet name ="
                      , "  " <> D.dullyellow "\"Hello \"" <> " ++ name ++ " <> D.dullyellow "\"!\""
                      ]
                  , D.reflow $
                      "Try to use that format with your `" ++ Name.toChars name ++ "` definition!"
                  ]
              )

        Code.Operator op ->
          let
            surroundings = A.Region startCur cur
            region = toKeywordRegion cur op
          in
          Report.Report "UNEXPECTED SYMBOL" region [] $
            Code.toSnippet source surroundings (Just region)
              (
                D.reflow $
                  "I was not expecting to see this symbol here:"
              ,
                D.stack
                  [ D.reflow $
                      "I am not sure what is going wrong exactly, so here is a valid\
                      \ definition (with an optional type annotation) for reference:"
                  , D.indent 4 $ D.vcat $
                      [ "greet : String -> String"
                      , "greet name ="
                      , "  " <> D.dullyellow "\"Hello \"" <> " ++ name ++ " <> D.dullyellow "\"!\""
                      ]
                  , D.reflow $
                      "Try to use that format with your `" ++ Name.toChars name ++ "` definition!"
                  ]
              )

        _ ->
          let
            surroundings = A.Region startCur cur
            region = toRegion cur
          in
          Report.Report "PROBLEM IN DEFINITION" region [] $
            Code.toSnippet source surroundings (Just region)
              (
                D.reflow $
                  "I got stuck while parsing the `" ++ Name.toChars name ++ "` definition:"
              ,
                D.stack
                  [ D.reflow $
                      "I am not sure what is going wrong exactly, so here is a valid\
                      \ definition (with an optional type annotation) for reference:"
                  , D.indent 4 $ D.vcat $
                      [ "greet : String -> String"
                      , "greet name ="
                      , "  " <> D.dullyellow "\"Hello \"" <> " ++ name ++ " <> D.dullyellow "\"!\""
                      ]
                  , D.reflow $
                      "Try to use that format!"
                  ]
              )

    DeclDefType tipe cur ->
      toTypeReport source (TC_Annotation name) tipe cur

    DeclDefArg pattern cur ->
      toPatternReport source PArg pattern cur

    DeclDefBody expr cur ->
      toExprReport source (InDef name startCur) expr cur

    DeclDefNameRepeat cur ->
      let
        surroundings = A.Region startCur cur
        region = toRegion cur
      in
      Report.Report "EXPECTING DEFINITION" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I just saw the type annotation for `" ++ Name.toChars name
              ++ "` so I was expecting to see its definition here:"
          ,
            D.stack
              [ D.reflow $
                  "Type annotations always appear directly above the relevant\
                  \ definition, without anything else in between. (Not even doc comments!)"
              , declDefNote
              ]
          )

    DeclDefNameMatch defName cur ->
      let
        surroundings = A.Region startCur cur
        region = toRegion cur
      in
      Report.Report "NAME MISMATCH" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I just saw a type annotation for `" ++ Name.toChars name ++ "`, but it is followed by a definition for `" ++ Name.toChars defName ++ "`:"
          ,
            D.stack
              [ D.reflow $
                  "These names do not match! Is there a typo?"
              , D.indent 4 $ D.fillSep $
                  [D.dullyellow (D.fromName defName),"->",D.green (D.fromName name)]
              ]
          )

    DeclDefIndentType cur ->
      let
        surroundings = A.Region startCur cur
        region = toRegion cur
      in
      Report.Report "UNFINISHED DEFINITION" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I got stuck while parsing the `" ++ Name.toChars name ++ "` type annotation:"
          ,
            D.stack
              [ D.reflow $
                  "I just saw a colon, so I am expecting to see a type next."
              , declDefNote
              ]
          )

    DeclDefIndentEquals cur ->
      let
        surroundings = A.Region startCur cur
        region = toRegion cur
      in
      Report.Report "UNFINISHED DEFINITION" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I got stuck while parsing the `" ++ Name.toChars name ++ "` definition:"
          ,
            D.stack
              [ D.reflow $
                  "I was expecting to see an argument or an equals sign next."
              , declDefNote
              ]
          )

    DeclDefIndentBody cur ->
      let
        surroundings = A.Region startCur cur
        region = toRegion cur
      in
      Report.Report "UNFINISHED DEFINITION" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I got stuck while parsing the `" ++ Name.toChars name ++ "` definition:"
          ,
            D.stack
              [ D.reflow $
                  "I was expecting to see an expression next. What is it equal to?"
              , declDefNote
              ]
          )


declDefNote :: D.Doc
declDefNote =
  D.stack
    [ D.reflow $
        "Here is a valid definition (with a type annotation) for reference:"
    , D.indent 4 $ D.vcat $
        [ "greet : String -> String"
        , "greet name ="
        , "  " <> D.dullyellow "\"Hello \"" <> " ++ name ++ " <> D.dullyellow "\"!\""
        ]
    , D.reflow $
        "The top line (called a \"type annotation\") is optional. You can leave it off\
        \ if you want. As you get more comfortable with Elm and as your project grows,\
        \ it becomes more and more valuable to add them though! They work great as\
        \ compiler-verified documentation, and they often improve error messages!"
    ]



-- CONTEXT


data Context
  = InNode Node Cursor Context
  | InDef Name.Name Cursor
  | InDestruct Cursor


data Node
  = NRecord
  | NParens
  | NList
  | NFunc
  | NCond
  | NThen
  | NElse
  | NCase
  | NBranch
  deriving (Eq)


getDefName :: Context -> Maybe Name.Name
getDefName context =
  case context of
    InDestruct _ -> Nothing
    InDef name _ -> Just name
    InNode _ _ c -> getDefName c


isWithin :: Node -> Context -> Bool
isWithin desiredNode context =
  case context of
    InDestruct _          -> False
    InDef _ _             -> False
    InNode actualNode _ _ -> desiredNode == actualNode



-- EXPR REPORTS


toExprReport :: Code.Source -> Context -> Expr -> Cursor -> Report.Report
toExprReport source context expr startCur =
  case expr of
    Let let_ cur ->
      toLetReport source context let_ cur

    Case case_ cur ->
      toCaseReport source context case_ cur

    If if_ cur ->
      toIfReport source context if_ cur

    List list cur ->
      toListReport source context list cur

    Record record cur ->
      toRecordReport source context record cur

    Tuple tuple cur ->
      toTupleReport source context tuple cur

    Func func cur ->
      toFuncReport source context func cur

    Dot cur ->
      let region = toRegion cur in
      Report.Report "EXPECTING RECORD ACCESSOR" region [] $
        Code.toSnippet source region Nothing
          (
            D.reflow $
              "I was expecting to see a record accessor here:"
          ,
            D.fillSep
              ["Something","like",D.dullyellow".name","or",D.dullyellow".price"
              ,"that","accesses","a","value","from","a","record."
              ]
          )

    Access cur ->
      let region = toRegion cur in
      Report.Report "EXPECTING RECORD ACCESSOR" region [] $
        Code.toSnippet source region Nothing
          (
            D.reflow $
              "I am trying to parse a record accessor here:"
          ,
            D.stack
              [
                D.fillSep
                  ["Something","like",D.dullyellow".name","or",D.dullyellow".price"
                  ,"that","accesses","a","value","from","a","record."
                  ]
              ,
                D.toSimpleNote $
                  "Record field names must start with a lower case letter!"
              ]
          )

    OperatorRight op cur ->
      let
        surroundings = A.Region startCur cur
        region = toRegion cur
        isMath = elem op ["-","+","*","/","^"]
      in
      Report.Report "MISSING EXPRESSION" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
                "I just saw a " ++ Name.toChars op ++ " "
                ++ (if isMath then "sign" else "operator")
                ++ ", so I am getting stuck here:"
          ,
            if isMath then
              D.fillSep
                ["I","was","expecting","to","see","an","expression","next."
                ,"Something","like",D.dullyellow "42","or",D.dullyellow "1000"
                ,"that","makes","sense","with","a",D.fromName op,"sign."
                ]
            else if op == "&&" || op == "||" then
              D.fillSep
                ["I","was","expecting","to","see","an","expression","next."
                ,"Something","like",D.dullyellow "True","or",D.dullyellow "False"
                ,"that","makes","sense","with","boolean","logic."
                ]
            else if op == "|>" then
              D.reflow $
                "I was expecting to see a function next."
            else if op == "<|" then
              D.reflow $
                "I was expecting to see an argument next."
            else
              D.reflow $
                "I was expecting to see an expression next."
          )

    OperatorReserved operator cur ->
      toOperatorReport source context operator cur

    Start cur ->
      let
        !(# contextCur, aThing #) =
          case context of
            InDestruct c       -> (# c, "a definition" #)
            InDef name c       -> (# c, "the `" ++ Name.toChars name ++ "` definition" #)
            InNode NRecord c _ -> (# c, "a record" #)
            InNode NParens c _ -> (# c, "some parentheses" #)
            InNode NList   c _ -> (# c, "a list" #)
            InNode NFunc   c _ -> (# c, "an anonymous function" #)
            InNode NCond   c _ -> (# c, "an `if` expression" #)
            InNode NThen   c _ -> (# c, "an `if` expression" #)
            InNode NElse   c _ -> (# c, "an `if` expression" #)
            InNode NCase   c _ -> (# c, "a `case` expression" #)
            InNode NBranch c _ -> (# c, "a `case` expression" #)

        surroundings = A.Region contextCur cur
        region = toRegion cur
      in
      Report.Report "MISSING EXPRESSION" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I am partway through parsing " ++ aThing ++ ", but I got stuck here:"
          ,
            D.stack
              [ D.fillSep $
                  ["I","was","expecting","to","see","an","expression","like"
                  ,D.dullyellow "42","or",D.dullyellow"\"hello\"" <> "."
                  ,"Once","there","is","something","there,","I","can","probably"
                  ,"give","a","more","specific","hint!"
                  ]
              , D.toSimpleNote $
                  "This can also happen if I run into reserved words like `let` or `as` unexpectedly.\
                  \ Or if I run into operators in unexpected spots. Point is, there are a\
                  \ couple ways I can get confused and give sort of weird advice!"
              ]
          )

    Char char cur ->
      toCharReport source char cur

    String string cur ->
      toStringReport source string cur

    Number number cur ->
      toNumberReport source number cur

    Space space cur ->
      toSpaceReport source space cur

    ShaderEndless cur ->
      let
        region = toRegion cur
      in
      Report.Report "ENDLESS SHADER" region [] $
        Code.toSnippet source region Nothing
          (
            D.reflow "I cannot find the end of this shader:"
          ,
            D.reflow "Add a |] somewhere after this to end the shader."
          )

    ShaderNotUtf8 cur ->
      let
        region = toRegion cur
      in
      Report.Report "UNEXPECTED ENCODING" region [] $
        Code.toSnippet source region Nothing
          (
            D.reflow $
              "Elm files use UTF-8 character encoding, but I ran into something outside of that format:"
          ,
            D.reflow $
              "Is there a way to convert this to a valid UTF-8 character?"
          )

    ShaderProblem problem cur ->
      let
        region = toRegion cur
      in
      Report.Report "SHADER PROBLEM" region [] $
        Code.toSnippet source region Nothing
          (
            D.reflow $
              "I ran into a problem while parsing this GLSL block."
          ,
            D.stack
              [ D.reflow $
                  "I use a 3rd party GLSL parser for now, and I did my best to extract their error message:"
              , D.indent 4 $ D.vcat $
                  map D.fromChars (filter (/="") (lines problem))
              ]
          )

    IndentOperatorRight op cur ->
      let
        surroundings = A.Region startCur cur
        region = toRegion cur
      in
      Report.Report "MISSING EXPRESSION" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I was expecting to see an expression after this " ++ Name.toChars op ++ " operator:"
          ,
            D.stack
              [
                D.fillSep $
                  ["You","can","just","put","anything","for","now,","like"
                  ,D.dullyellow "42","or",D.dullyellow"\"hello\"" <> "."
                  ,"Once","there","is","something","there,","I","can","probably"
                  ,"give","a","more","specific","hint!"
                  ]
              ,
                D.toSimpleNote $
                  "I may be getting confused by your indentation? The easiest way to make sure\
                  \ this is not an indentation problem is to put the expression on the right of\
                  \ the " ++ Name.toChars op ++ " operator on the same line."
              ]
          )



-- CHAR


toCharReport :: Code.Source -> Char -> Cursor -> Report.Report
toCharReport source char cur =
  case char of
    CharNotUtf8 ->
      let
        region = toRegion cur
      in
      Report.Report "UNEXPECTED ENCODING" region [] $
        Code.toSnippet source region Nothing
          (
            D.reflow $
              "Elm files use UTF-8 character encoding, but I ran into something outside of that format:"
          ,
            D.reflow $
              "Is there a way to convert this to a valid UTF-8 character?"
          )

    CharEndless ->
      let
        region = toRegion cur
      in
      Report.Report "MISSING SINGLE QUOTE" region [] $
        Code.toSnippet source region Nothing
          (
            D.reflow $
              "I thought I was parsing a character, but I got to the end of\
              \ the line without seeing the closing single quote:"
          ,
            D.reflow $
              "Add a closing single quote here!"
          )

    CharEscape escape ->
      toEscapeReport source escape cur

    CharNotString width ->
      let
        region = toWiderRegion cur width
      in
      Report.Report "NEEDS DOUBLE QUOTES" region [] $
        Code.toSnippet source region Nothing
          (
            "The following string uses single quotes:"
          ,
            D.stack
              [ "Please switch to double quotes instead:"
              , D.indent 4 $
                  D.dullyellow "'this'" <> " => " <> D.green "\"this\""
              , D.toSimpleNote $
                  "Elm uses double quotes for strings like \"hello\", whereas it uses single\
                  \ quotes for individual characters like 'a' and 'ø'. This distinction helps with\
                  \ code like (String.any (\\c -> c == 'X') \"90210\") where you are inspecting\
                  \ individual characters."
              ]
          )



-- STRING


toStringReport :: Code.Source -> String -> Cursor -> Report.Report
toStringReport source string cur =
  case string of
    StringNotUtf8 ->
      let
        region = toRegion cur
      in
      Report.Report "UNEXPECTED ENCODING" region [] $
        Code.toSnippet source region Nothing
          (
            D.reflow $
              "Elm files use UTF-8 character encoding, but I ran into something outside of that format:"
          ,
            D.reflow $
              "Is there a way to convert this to a valid UTF-8 character?"
          )

    StringEndless_Single ->
      let
        region = toRegion cur
      in
      Report.Report "ENDLESS STRING" region [] $
        Code.toSnippet source region Nothing
          (
            D.reflow $
              "I got to the end of the line without seeing the closing double quote:"
          ,
            D.stack
              [ D.fillSep $
                  ["Strings","look","like",D.green "\"this\"","with","double"
                  ,"quotes","on","each","end.","Is","the","closing","double"
                  ,"quote","missing","in","your","code?"
                  ]
              , D.toSimpleNote $
                  "For a string that spans multiple lines, you can use the multi-line string\
                  \ syntax like this:"
              , D.dullyellow $ D.indent 4 $ D.vcat $
                  [ "\"\"\""
                  , "# Multi-line Strings"
                  , ""
                  , "- start with triple double quotes"
                  , "- write whatever you want"
                  , "- no need to escape newlines or double quotes"
                  , "- end with triple double quotes"
                  , "\"\"\""
                  ]
              ]
          )

    StringEndless_Multi ->
      let
        region = toWiderRegion cur 3#Word64
      in
      Report.Report "ENDLESS STRING" region [] $
        Code.toSnippet source region Nothing
          (
            D.reflow $
              "I cannot find the end of this multi-line string:"
          ,
            D.stack
              [ D.reflow "Add a \"\"\" somewhere after this to end the string."
              , D.toSimpleNote $
                  "Here is a valid multi-line string for reference:"
              , D.dullyellow $ D.indent 4 $ D.vcat $
                  [ "\"\"\""
                  , "# Multi-line Strings"
                  , ""
                  , "- start with triple double quotes"
                  , "- write whatever you want"
                  , "- no need to escape newlines or double quotes"
                  , "- end with triple double quotes"
                  , "\"\"\""
                  ]
              ]
          )

    StringEscape escape ->
      toEscapeReport source escape cur



-- ESCAPES


toEscapeReport :: Code.Source -> Escape -> Cursor -> Report.Report
toEscapeReport source escape cur =
  case escape of
    EscapeUnknown ->
      let
        region = toWiderRegion cur 2#Word64
      in
      Report.Report "UNKNOWN ESCAPE" region [] $
        Code.toSnippet source region Nothing
          (
            D.reflow $
              "Backslashes always start escaped characters, but I do not recognize this one:"
          ,
            D.stack
              [ D.reflow $
                  "Valid escape characters include:"
              , D.dullyellow $ D.indent 4 $ D.vcat $
                    [ "\\n"
                    , "\\r"
                    , "\\t"
                    , "\\\""
                    , "\\\'"
                    , "\\\\"
                    , "\\u{003D}"
                    ]
              , D.reflow $
                  "Do you want one of those instead? Maybe you need \\\\ to escape a backslash?"
              , D.toSimpleNote $
                  "The last style lets encode ANY character by its Unicode code\
                  \ point. That means \\u{0009} and \\t are the same. You can use\
                  \ that style for anything not covered by the other six escapes!"
              ]
          )

    BadUnicodeFormat width ->
      let
        region = toWiderRegion cur width
      in
      Report.Report "BAD UNICODE ESCAPE" region [] $
        Code.toSnippet source region Nothing
          (
            D.reflow $
              "I ran into an invalid Unicode escape:"
          ,
            D.stack
              [ D.reflow $
                  "Here are some examples of valid Unicode escapes:"
              , D.dullyellow $ D.indent 4 $ D.vcat $
                  [ "\\u{0041}"
                  , "\\u{03BB}"
                  , "\\u{6728}"
                  , "\\u{1F60A}"
                  ]
              , D.reflow $
                  "Notice that the code point is always surrounded by curly braces.\
                  \ Maybe you are missing the opening or closing curly brace?"
              ]
            )
    BadUnicodeCode width ->
      let
        region = toWiderRegion cur width
      in
      Report.Report "BAD UNICODE ESCAPE" region [] $
        Code.toSnippet source region Nothing
          (
            D.reflow $
              "This is not a valid code point:"
          ,
            D.reflow $
              "The valid code points are between 0 and 10FFFF inclusive."
          )

    BadUnicodeLength width numDigits badCode ->
      let
        region = toWiderRegion cur width
      in
      Report.Report "BAD UNICODE ESCAPE" region [] $
        Code.toSnippet source region Nothing $
          if numDigits < 4 then
            (
              D.reflow $
                "Every code point needs at least four digits:"
            ,
              let
                goodCode = replicate (4 - numDigits) '0' ++ map Char.toUpper (showHex badCode "")
                suggestion = "\\u{" <> D.fromChars goodCode <> "}"
              in
              D.fillSep ["Try",D.green suggestion,"instead?"]
            )

          else
            (
              D.reflow $
                "This code point has too many digits:"
            ,
              D.fillSep $
                ["Valid","code","points","are","between"
                ,D.green "\\u{0000}","and",D.green "\\u{10FFFF}" <> ","
                ,"so","try","trimming","any","leading","zeros","until"
                ,"you","have","between","four","and","six","digits."
                ]
            )



-- NUMBERS


toNumberReport :: Code.Source -> Number -> Cursor -> Report.Report
toNumberReport source number cur =
  let
    region = toRegion cur
  in
  case number of
    NumberEnd ->
      Report.Report "WEIRD NUMBER" region [] $
        Code.toSnippet source region Nothing
          (
            D.reflow $
              "I thought I was reading a number, but I ran into some weird stuff here:"
          ,
            D.stack
              [ D.reflow $
                  "I recognize numbers in the following formats:"
              , D.indent 4 $ D.vcat [ "42", "3.14", "6.022e23", "0x002B" ]
              , D.reflow $
                  "So is there a way to write it like one of those?"
              ]
          )

    NumberDot int ->
      Report.Report "WEIRD NUMBER" region [] $
        Code.toSnippet source region Nothing
          (
            D.reflow $
              "Numbers cannot end with a dot like this:"
          ,
            D.fillSep
              ["Switching","to",D.green (D.fromChars (show int))
              ,"or",D.green (D.fromChars (show int ++ ".0"))
              ,"will","work","though!"
              ]
          )

    NumberHexDigit ->
      Report.Report "WEIRD HEXIDECIMAL" region [] $
        Code.toSnippet source region Nothing
          (
            D.reflow $
              "I thought I was reading a hexidecimal number until I got here:"
          ,
            D.stack
              [ D.reflow $
                  "Valid hexidecimal digits include 0123456789abcdefABCDEF, so I can\
                  \ only recognize things like this:"
              , D.indent 4 $ D.vcat [ "0x2B", "0x002B", "0x00ffb3" ]
              ]
          )

    NumberNoLeadingZero ->
      Report.Report "LEADING ZEROS" region [] $
        Code.toSnippet source region Nothing
          (
            D.reflow $
              "I do not accept numbers with leading zeros:"
          ,
            D.stack
              [ D.reflow $
                  "Just delete the leading zeros and it should work!"
              , D.toSimpleNote $
                  "Some languages let you to specify octal numbers by adding a leading zero.\
                  \ So in C, writing 0111 is the same as writing 73. Some people are used to\
                  \ that, but others probably want it to equal 111. Either path is going to\
                  \ surprise people from certain backgrounds, so Elm tries to avoid this whole\
                  \ situation."
              ]
          )



-- OPERATORS


toOperatorReport :: Code.Source -> Context -> BadOperator -> Cursor -> Report.Report
toOperatorReport source context operator cur =
  case operator of
    BadDot ->
      let
        region = toRegion cur
      in
      Report.Report "UNEXPECTED SYMBOL" region [] $
        Code.toSnippet source region Nothing
          (
            "I was not expecting this dot:"
          ,
            D.reflow $
              "Dots are for record access and decimal points, so\
              \ they cannot float around on their own. Maybe\
              \ there is some extra whitespace?"
          )

    BadPipe ->
      let
        region = toRegion cur
      in
      Report.Report "UNEXPECTED SYMBOL" region [] $
        Code.toSnippet source region Nothing
          (
            D.reflow $
              "I was not expecting this vertical bar:"
          ,
            D.reflow $
              "Vertical bars should only appear in custom type declarations. Maybe you want || instead?"
          )

    BadArrow ->
      let
        region = toWiderRegion cur 2#Word64
      in
      Report.Report "UNEXPECTED ARROW" region [] $
        Code.toSnippet source region Nothing $
          if isWithin NCase context then
            (
              D.reflow $
                "I am parsing a `case` expression right now, but this arrow is confusing me:"
            ,
              D.stack
                [ D.reflow "Maybe the `of` keyword is missing on a previous line?"
                , noteForCaseError
                ]
            )

          else if isWithin NBranch context then
            (
              D.reflow $
                "I am parsing a `case` expression right now, but this arrow is confusing me:"
            ,
              D.stack
                [ D.reflow $
                    "It makes sense to see arrows around here, so I suspect it is something earlier. Maybe this pattern is indented a bit farther than the previous patterns?"
                , noteForCaseIndentError
                ]
            )

          else
            (
              D.reflow $
                "I was partway through parsing an expression when I got stuck on this arrow:"
            ,
              D.stack
                [ "Arrows should only appear in `case` expressions and anonymous functions.\n\
                  \Maybe it was supposed to be a > sign instead?"
                , D.toSimpleNote $
                    "The syntax for anonymous functions is (\\x -> x + 1) so the arguments all appear\
                    \ after the backslash and before the arrow. Maybe a backslash is missing earlier?"
                ]
            )

    BadEquals ->
      let
        region = toRegion cur
      in
      Report.Report "UNEXPECTED EQUALS" region [] $
        Code.toSnippet source region Nothing
          (
            D.reflow $
              "I was not expecting to see this equals sign:"
          ,
            D.stack
              [
                D.reflow "Maybe you want == instead? To check if two values are equal?"
              ,
                D.toSimpleNote $
                  if isWithin NRecord context then
                    "Records look like { x = 3, y = 4 } with the equals sign right\
                    \ after the field name. So maybe you forgot a comma?"
                  else
                    case getDefName context of
                      Nothing ->
                        "I may be getting confused by your indentation. I need all definitions to be indented\
                        \ exactly the same amount, so if this is meant to be a new definition, it may have too\
                        \ many spaces in front of it."

                      Just name ->
                        "I may be getting confused by your indentation. I think I am still parsing the `"
                        ++ Name.toChars name ++ "` definition. Is this supposed to be part of a definition\
                        \ after that? If so, the problem may be a bit before the equals sign. I need all\
                        \ definitions to be indented exactly the same amount, so the problem may be that\
                        \ this new definition has too many spaces in front of it."
              ]
          )

    BadHasType ->
      let
        region = toRegion cur
      in
      Report.Report "UNEXPECTED SYMBOL" region [] $
        Code.toSnippet source region Nothing $
          (
            D.reflow $
              "I was not expecting to run into the \"has type\" symbol here:"
          ,
            case getDefName context of
              Nothing ->
                D.fillSep
                  ["Maybe","you","want",D.green "::","instead?"
                  ,"To","put","something","on","the","front","of","a","list?"
                  ]

              Just name ->
                D.stack
                  [
                    D.fillSep
                      ["Maybe","you","want",D.green "::","instead?"
                      ,"To","put","something","on","the","front","of","a","list?"
                      ]
                  , D.toSimpleNote $
                      "The single colon is reserved for type annotations and record types, but I think\
                      \ I am parsing the definition of `" ++ Name.toChars name ++ "` right now."
                  ,
                    D.toSimpleNote $
                      "I may be getting confused by your indentation. Is this supposed to be part of\
                      \ a type annotation AFTER the `" ++ Name.toChars name ++ "` definition? If so,\
                      \ the problem may be a bit before the \"has type\" symbol. I need all definitions to\
                      \ be exactly aligned (with exactly the same indentation) so the problem may be that\
                      \ this new definition is indented a bit too much."
                  ]
          )



-- CASE


toLetReport :: Code.Source -> Context -> Let -> Cursor -> Report.Report
toLetReport source context let_ startCur =
  case let_ of
    LetSpace space cur ->
      toSpaceReport source space cur

    LetIn cur ->
      let
        surroundings = A.Region startCur cur
        region = toRegion cur
      in
      Report.Report "LET PROBLEM" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I was partway through parsing a `let` expression, but I got stuck here:"
          ,
            D.stack
              [ D.fillSep $
                  ["Based","on","the","indentation,","I","was","expecting","to","see","the",D.cyan "in"
                  ,"keyword","next.","Is","there","a","typo?"
                  ]
              , D.toSimpleNote $
                  "This can also happen if you are trying to define another value within the `let` but\
                  \ it is not indented enough. Make sure each definition has exactly the same amount of\
                  \ spaces before it. They should line up exactly!"
              ]
          )

    LetDefAlignment _ cur ->
      let
        surroundings = A.Region startCur cur
        region = toRegion cur
      in
      Report.Report "LET PROBLEM" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I was partway through parsing a `let` expression, but I got stuck here:"
          ,
            D.stack
              [ D.fillSep $
                  ["Based","on","the","indentation,","I","was","expecting","to","see","the",D.cyan "in"
                  ,"keyword","next.","Is","there","a","typo?"
                  ]
              , D.toSimpleNote $
                  "This can also happen if you are trying to define another value within the `let` but\
                  \ it is not indented enough. Make sure each definition has exactly the same amount of\
                  \ spaces before it. They should line up exactly!"
              ]
          )

    LetDefName cur ->
      case Code.whatIsNext source cur of
        Code.Keyword keyword ->
          let
            surroundings = A.Region startCur cur
            region = toKeywordRegion cur keyword
          in
          Report.Report "RESERVED WORD" region [] $
            Code.toSnippet source surroundings (Just region)
              (
                D.reflow $
                  "I was partway through parsing a `let` expression, but I got stuck here:"
              ,
                D.reflow $
                  "It looks like you are trying to use `" ++ keyword ++ "` as a variable name, but\
                  \ it is a reserved word! Try using a different name instead."
              )

        _ ->
          toUnfinishLetReport source cur startCur $
            D.reflow $
              "I was expecting the name of a definition next."

    LetDef name def cur ->
      toLetDefReport source name def cur

    LetDestruct destruct cur ->
      toLetDestructReport source destruct cur

    LetBody expr cur ->
      toExprReport source context expr cur

    LetIndentDef cur ->
      toUnfinishLetReport source cur startCur $
        D.reflow $
          "I was expecting a value to be defined here."

    LetIndentIn cur ->
      toUnfinishLetReport source cur startCur $
        D.fillSep $
          ["I","was","expecting","to","see","the",D.cyan "in","keyword","next."
          ,"Or","maybe","more","of","that","expression?"
          ]

    LetIndentBody cur ->
      toUnfinishLetReport source cur startCur $
        D.reflow $
          "I was expecting an expression next. Tell me what should happen with the value you just defined!"


toUnfinishLetReport :: Code.Source -> Cursor -> Cursor -> D.Doc -> Report.Report
toUnfinishLetReport source cur startCur message =
  let
    surroundings = A.Region startCur cur
    region = toRegion cur
  in
  Report.Report "UNFINISHED LET" region [] $
    Code.toSnippet source surroundings (Just region)
      (
        D.reflow $
          "I was partway through parsing a `let` expression, but I got stuck here:"
      ,
        D.stack
          [ message
          , D.toSimpleNote $
              "Here is an example with a valid `let` expression for reference:"
          , D.indent 4 $ D.vcat $
              [ D.indent 0 $ D.fillSep ["viewPerson","person","="]
              , D.indent 2 $ D.fillSep [D.cyan "let"]
              , D.indent 4 $ D.fillSep ["fullName","="]
              , D.indent 6 $ D.fillSep ["person.firstName","++",D.dullyellow "\" \"","++","person.lastName"]
              , D.indent 2 $ D.fillSep [D.cyan "in"]
              , D.indent 2 $ D.fillSep ["div","[]","[","text","fullName","]"]
              ]
          , D.reflow $
              "Here we defined a `viewPerson` function that turns a person into some HTML. We use\
              \ a `let` expression to define the `fullName` we want to show. Notice the indentation! The\
              \ `fullName` is indented more than the `let` keyword, and the actual value of `fullName` is\
              \ indented a bit more than that. That is important!"
          ]
      )


toLetDefReport :: Code.Source -> Name.Name -> Def -> Cursor -> Report.Report
toLetDefReport source name def startCur =
  case def of
    DefSpace space cur ->
      toSpaceReport source space cur

    DefType tipe cur ->
      toTypeReport source (TC_Annotation name) tipe cur

    DefNameRepeat cur ->
      let
        surroundings = A.Region startCur cur
        region = toRegion cur
      in
      Report.Report "EXPECTING DEFINITION" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I just saw the type annotation for `" ++ Name.toChars name
              ++ "` so I was expecting to see its definition here:"
          ,
            D.stack
              [ D.reflow $
                  "Type annotations always appear directly above the relevant\
                  \ definition, without anything else in between."
              , defNote
              ]
          )

    DefNameMatch defName cur ->
      let
        surroundings = A.Region startCur cur
        region = toRegion cur
      in
      Report.Report "NAME MISMATCH" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I just saw a type annotation for `" ++ Name.toChars name ++ "`, but it is followed by a definition for `" ++ Name.toChars defName ++ "`:"
          ,
            D.stack
              [ D.reflow $
                  "These names do not match! Is there a typo?"
              , D.indent 4 $ D.fillSep $
                  [D.dullyellow (D.fromName defName),"->",D.green (D.fromName name)]
              ]
          )

    DefArg pattern cur ->
      toPatternReport source PArg pattern cur

    DefEquals cur ->
      case Code.whatIsNext source cur of
        Code.Keyword keyword ->
          let
            surroundings = A.Region startCur cur
            region = toKeywordRegion cur keyword
          in
          Report.Report "RESERVED WORD" region [] $
            Code.toSnippet source surroundings (Just region)
              (
                D.fillSep
                  ["The","name"
                  ,"`" <> D.cyan (D.fromChars keyword) <> "`"
                  ,"is","reserved","in","Elm,","so","it","cannot"
                  ,"be","used","as","an","argument","here:"
                  ]
              ,
                D.stack
                  [ D.reflow $
                      "Try renaming it to something else."
                  , case keyword of
                      "as" ->
                        D.toFancyNote
                          ["This","keyword","is","reserved","for","pattern","matches","like"
                          ,"((x,y)",D.cyan "as","point)","where","you","want","to","name","a","tuple","and"
                          ,"the","values","it","contains."
                          ]

                      _ ->
                        D.toSimpleNote $
                          "The `" ++ keyword ++ "` keyword has a special meaning in Elm, so it can only be used in certain situations."
                  ]
              )

        Code.Operator "->" ->
          let
            surroundings = A.Region startCur cur
            region = toWiderRegion cur 2#Word64
          in
          Report.Report "MISSING COLON?" region [] $
            Code.toSnippet source surroundings (Just region)
              (
                D.reflow $
                  "I was not expecting to see an arrow here:"
              ,
                D.stack
                  [ D.fillSep
                      ["This","usually","means","a",D.green ":","is","missing","a","bit","earlier","in"
                      ,"a","type","annotation.","It","could","be","something","else","though,","so"
                      ,"here","is","a","valid","definition","for","reference:"
                      ]
                  , D.indent 4 $ D.vcat $
                      [ "greet : String -> String"
                      , "greet name ="
                      , "  " <> D.dullyellow "\"Hello \"" <> " ++ name ++ " <> D.dullyellow "\"!\""
                      ]
                  , D.reflow $
                      "Try to use that format with your `" ++ Name.toChars name ++ "` definition!"
                  ]
              )

        Code.Operator op ->
          let
            surroundings = A.Region startCur cur
            region = toKeywordRegion cur op
          in
          Report.Report "UNEXPECTED SYMBOL" region [] $
            Code.toSnippet source surroundings (Just region)
              (
                D.reflow $
                  "I was not expecting to see this symbol here:"
              ,
                D.stack
                  [ D.reflow $
                      "I am not sure what is going wrong exactly, so here is a valid\
                      \ definition (with an optional type annotation) for reference:"
                  , D.indent 4 $ D.vcat $
                      [ "greet : String -> String"
                      , "greet name ="
                      , "  " <> D.dullyellow "\"Hello \"" <> " ++ name ++ " <> D.dullyellow "\"!\""
                      ]
                  , D.reflow $
                      "Try to use that format with your `" ++ Name.toChars name ++ "` definition!"
                  ]
              )

        _ ->
          let
            surroundings = A.Region startCur cur
            region = toRegion cur
          in
          Report.Report "PROBLEM IN DEFINITION" region [] $
            Code.toSnippet source surroundings (Just region)
              (
                D.reflow $
                  "I got stuck while parsing the `" ++ Name.toChars name ++ "` definition:"
              ,
                D.stack
                  [ D.reflow $
                      "I am not sure what is going wrong exactly, so here is a valid\
                      \ definition (with an optional type annotation) for reference:"
                  , D.indent 4 $ D.vcat $
                      [ "greet : String -> String"
                      , "greet name ="
                      , "  " <> D.dullyellow "\"Hello \"" <> " ++ name ++ " <> D.dullyellow "\"!\""
                      ]
                  , D.reflow $
                      "Try to use that format!"
                  ]
              )

    DefBody expr cur ->
      toExprReport source (InDef name startCur) expr cur

    DefIndentEquals cur ->
      let
        surroundings = A.Region startCur cur
        region = toRegion cur
      in
      Report.Report "UNFINISHED DEFINITION" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I got stuck while parsing the `" ++ Name.toChars name ++ "` definition:"
          ,
            D.stack
              [ D.reflow $
                  "I was expecting to see an argument or an equals sign next."
              , defNote
              ]
          )

    DefIndentType cur ->
      let
        surroundings = A.Region startCur cur
        region = toRegion cur
      in
      Report.Report "UNFINISHED DEFINITION" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I got stuck while parsing the `" ++ Name.toChars name ++ "` type annotation:"
          ,
            D.stack
              [ D.reflow $
                  "I just saw a colon, so I am expecting to see a type next."
              , defNote
              ]
          )

    DefIndentBody cur ->
      let
        surroundings = A.Region startCur cur
        region = toRegion cur
      in
      Report.Report "UNFINISHED DEFINITION" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I got stuck while parsing the `" ++ Name.toChars name ++ "` definition:"
          ,
            D.stack
              [ D.reflow $
                  "I was expecting to see an expression next. What is it equal to?"
              , declDefNote
              ]
          )

    DefAlignment indent cur ->
      let
        surroundings = A.Region startCur cur
        region = toRegion cur
        offset = distanceToIndent indent cur
      in
      Report.Report "PROBLEM IN DEFINITION" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I got stuck while parsing the `" ++ Name.toChars name ++ "` definition:"
          ,
            D.reflow $
              "I just saw a type annotation indented " ++ show (W32# indent) ++ " spaces, so I was\
              \ expecting to see the corresponding definition next with the exact same amount\
              \ of indentation. It looks like this line needs "
              ++ show offset ++ " more " ++ (if offset == 1 then "space" else "spaces") ++ "?"
          )



defNote :: D.Doc
defNote =
  D.stack
    [ D.reflow $
        "Here is a valid definition (with a type annotation) for reference:"
    , D.indent 4 $ D.vcat $
        [ "greet : String -> String"
        , "greet name ="
        , "  " <> D.dullyellow "\"Hello \"" <> " ++ name ++ " <> D.dullyellow "\"!\""
        ]
    , D.reflow $
        "The top line (called a \"type annotation\") is optional. You can leave it off\
        \ if you want. As you get more comfortable with Elm and as your project grows,\
        \ it becomes more and more valuable to add them though! They work great as\
        \ compiler-verified documentation, and they often improve error messages!"
    ]


toLetDestructReport :: Code.Source -> Destruct -> Cursor -> Report.Report
toLetDestructReport source destruct startCur =
  case destruct of
    DestructSpace space cur ->
      toSpaceReport source space cur

    DestructPattern pattern cur ->
      toPatternReport source PLet pattern cur

    DestructEquals cur ->
      let
        surroundings = A.Region startCur cur
        region = toRegion cur
      in
      Report.Report "PROBLEM IN DEFINITION" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I got stuck trying to parse this definition:"
          ,
            case Code.whatIsNext source cur of
              Code.Operator ":" ->
                D.stack
                  [ D.reflow $
                      "I was expecting to see an equals sign next, followed by an expression\
                      \ telling me what to compute."
                  , D.toSimpleNote $
                      "It looks like you may be trying to write a type annotation? It is not\
                      \ possible to add type annotations on destructuring definitions like this.\
                      \ You can assign a name to the overall structure, put a type annotation on\
                      \ that, and then destructure separately though."
                  ]

              _ ->
                D.reflow $
                  "I was expecting to see an equals sign next, followed by an expression\
                  \ telling me what to compute."
          )

    DestructBody expr cur ->
      toExprReport source (InDestruct startCur) expr cur

    DestructIndentEquals cur ->
      let
        surroundings = A.Region startCur cur
        region = toRegion cur
      in
      Report.Report "UNFINISHED DEFINITION" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I got stuck trying to parse this definition:"
          ,
            D.reflow $
              "I was expecting to see an equals sign next, followed by an expression\
              \ telling me what to compute."
          )

    DestructIndentBody cur ->
      let
        surroundings = A.Region startCur cur
        region = toRegion cur
      in
      Report.Report "UNFINISHED DEFINITION" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I got stuck while parsing this definition:"
          ,
            D.reflow $
              "I was expecting to see an expression next. What is it equal to?"
          )



-- CASE


toCaseReport :: Code.Source -> Context -> Case -> Cursor -> Report.Report
toCaseReport source context case_ startCur =
  case case_ of
    CaseSpace space cur ->
      toSpaceReport source space cur

    CaseOf cur ->
      toUnfinishCaseReport source cur startCur $
        D.fillSep ["I","was","expecting","to","see","the",D.dullyellow "of","keyword","next."]

    CasePattern pattern cur ->
      toPatternReport source PCase pattern cur

    CaseArrow cur ->
      case Code.whatIsNext source cur of
        Code.Keyword keyword ->
          let
            surroundings = A.Region startCur cur
            region = toKeywordRegion cur keyword
          in
          Report.Report "RESERVED WORD" region [] $
            Code.toSnippet source surroundings (Just region)
              (
                D.reflow $
                  "I am partway through parsing a `case` expression, but I got stuck here:"
              ,
                D.reflow $
                  "It looks like you are trying to use `" ++ keyword ++ "` in one of your\
                  \ patterns, but it is a reserved word. Try using a different name?"
              )

        Code.Operator ":" ->
          let
            surroundings = A.Region startCur cur
            region = toRegion cur
          in
          Report.Report "UNEXPECTED OPERATOR" region [] $
            Code.toSnippet source surroundings (Just region)
              (
                D.reflow $
                  "I am partway through parsing a `case` expression, but I got stuck here:"
              ,
                D.fillSep $
                  ["I","am","seeing",D.dullyellow ":","but","maybe","you","want",D.green "::","instead?"
                  ,"For","pattern","matching","on","lists?"
                  ]
              )

        Code.Operator "=" ->
          let
            surroundings = A.Region startCur cur
            region = toRegion cur
          in
          Report.Report "UNEXPECTED OPERATOR" region [] $
            Code.toSnippet source surroundings (Just region)
              (
                D.reflow $
                  "I am partway through parsing a `case` expression, but I got stuck here:"
              ,
                D.fillSep $
                  ["I","am","seeing",D.dullyellow "=","but","maybe","you","want",D.green "->","instead?"
                  ]
              )

        _ ->
          let
            surroundings = A.Region startCur cur
            region = toRegion cur
          in
          Report.Report "MISSING ARROW" region [] $
            Code.toSnippet source surroundings (Just region)
              (
                D.reflow $
                  "I am partway through parsing a `case` expression, but I got stuck here:"
              ,
                D.stack
                  [ D.reflow "I was expecting to see an arrow next."
                  , noteForCaseIndentError
                  ]
              )

    CaseExpr expr cur ->
      toExprReport source (InNode NCase startCur context) expr cur

    CaseBranch expr cur ->
      toExprReport source (InNode NBranch startCur context) expr cur

    CaseIndentOf cur ->
      toUnfinishCaseReport source cur startCur $
        D.fillSep ["I","was","expecting","to","see","the",D.dullyellow "of","keyword","next."]

    CaseIndentExpr cur ->
      toUnfinishCaseReport source cur startCur $
        D.reflow "I was expecting to see a expression next."

    CaseIndentPattern cur ->
      toUnfinishCaseReport source cur startCur $
        D.reflow "I was expecting to see a pattern next."

    CaseIndentArrow cur ->
      toUnfinishCaseReport source cur startCur $
        D.fillSep
          ["I","just","saw","a","pattern,","so","I","was","expecting"
          ,"to","see","a",D.dullyellow "->","next."
          ]

    CaseIndentBranch cur ->
      toUnfinishCaseReport source cur startCur $
        D.reflow $
          "I was expecting to see an expression next. What should I do when\
          \ I run into this particular pattern?"

    CasePatternAlignment indent cur ->
      toUnfinishCaseReport source cur startCur $
        D.reflow $
          "I suspect this is a pattern that is not indented far enough? (" ++ show (W32# indent) ++ " spaces)"


toUnfinishCaseReport :: Code.Source -> Cursor -> Cursor -> D.Doc -> Report.Report
toUnfinishCaseReport source cur startCur message =
  let
    surroundings = A.Region startCur cur
    region = toRegion cur
  in
  Report.Report "UNFINISHED CASE" region [] $
    Code.toSnippet source surroundings (Just region)
      (
        D.reflow $
          "I was partway through parsing a `case` expression, but I got stuck here:"
      ,
        D.stack
          [ message
          , noteForCaseError
          ]
      )


noteForCaseError :: D.Doc
noteForCaseError =
  D.stack
    [ D.toSimpleNote $
        "Here is an example of a valid `case` expression for reference."
    , D.vcat $
        [ D.indent 4 $ D.fillSep [D.cyan "case","maybeWidth",D.cyan "of"]
        , D.indent 6 $ D.fillSep [D.blue "Just","width","->"]
        , D.indent 8 $ D.fillSep ["width","+",D.dullyellow "200"]
        , ""
        , D.indent 6 $ D.fillSep [D.blue "Nothing","->"]
        , D.indent 8 $ D.fillSep [D.dullyellow "400"]
        ]
    , D.reflow $
        "Notice the indentation. Each pattern is aligned, and each branch is indented\
        \ a bit more than the corresponding pattern. That is important!"
    ]


noteForCaseIndentError :: D.Doc
noteForCaseIndentError =
  D.stack
    [ D.toSimpleNote $
        "Sometimes I get confused by indentation, so try to make your `case` look\
        \ something like this:"
    , D.vcat $
        [ D.indent 4 $ D.fillSep [D.cyan "case","maybeWidth",D.cyan "of"]
        , D.indent 6 $ D.fillSep [D.blue "Just","width","->"]
        , D.indent 8 $ D.fillSep ["width","+",D.dullyellow "200"]
        , ""
        , D.indent 6 $ D.fillSep [D.blue "Nothing","->"]
        , D.indent 8 $ D.fillSep [D.dullyellow "400"]
        ]
    , D.reflow $
        "Notice the indentation! Patterns are aligned with each other. Same indentation.\
        \ The expressions after each arrow are all indented a bit more than the patterns.\
        \ That is important!"
    ]



-- IF


toIfReport :: Code.Source -> Context -> If -> Cursor -> Report.Report
toIfReport source context if_ startCur =
  case if_ of
    IfSpace space cur ->
      toSpaceReport source space cur

    IfThen cur ->
      let
        surroundings = A.Region startCur cur
        region = toRegion cur
      in
      Report.Report "UNFINISHED IF" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I was expecting to see more of this `if` expression, but I got stuck here:"
          ,
            D.fillSep $
              ["I","was","expecting","to","see","the",D.cyan "then","keyword","next."
              ]
          )

    IfElse cur ->
      let
        surroundings = A.Region startCur cur
        region = toRegion cur
      in
      Report.Report "UNFINISHED IF" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I was expecting to see more of this `if` expression, but I got stuck here:"
          ,
            D.fillSep $
              ["I","was","expecting","to","see","the",D.cyan "else","keyword","next."
              ]
          )

    IfElseBranchStart cur ->
      let
        surroundings = A.Region startCur cur
        region = toRegion cur
      in
      Report.Report "UNFINISHED IF" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I just saw the start of an `else` branch, but then I got stuck here:"
          ,
            D.reflow $
              "I was expecting to see an expression next. Maybe it is not filled in yet?"
          )

    IfCondition expr cur ->
      toExprReport source (InNode NCond startCur context) expr cur

    IfThenBranch expr cur ->
      toExprReport source (InNode NThen startCur context) expr cur

    IfElseBranch expr cur ->
      toExprReport source (InNode NElse startCur context) expr cur

    IfIndentCondition cur ->
      let
        surroundings = A.Region startCur cur
        region = toRegion cur
      in
      Report.Report "UNFINISHED IF" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I was expecting to see more of this `if` expression, but I got stuck here:"
          ,
            D.stack
              [ D.fillSep $
                  ["I","was","expecting","to","see","an","expression","like",D.dullyellow "x < 0"
                  ,"that","evaluates","to","True","or","False."
                  ]
              , D.toSimpleNote $
                  "I can be confused by indentation. Maybe something is not indented enough?"
              ]
          )

    IfIndentThen cur ->
      let
        surroundings = A.Region startCur cur
        region = toRegion cur
      in
      Report.Report "UNFINISHED IF" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I was expecting to see more of this `if` expression, but I got stuck here:"
          ,
            D.stack
              [ D.fillSep $
                  ["I","was","expecting","to","see","the",D.cyan "then","keyword","next."
                  ]
              , D.toSimpleNote $
                  "I can be confused by indentation. Maybe something is not indented enough?"
              ]
          )

    IfIndentThenBranch cur ->
      let
        surroundings = A.Region startCur cur
        region = toRegion cur
      in
      Report.Report "UNFINISHED IF" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I got stuck after the start of this `then` branch:"
          ,
            D.stack
              [ D.reflow $
                  "I was expecting to see an expression next. Maybe it is not filled in yet?"
              , D.toSimpleNote $
                  "I can be confused by indentation, so if the `then` branch is already\
                  \ present, it may not be indented enough for me to recognize it."
              ]
          )

    IfIndentElseBranch cur ->
      let
        surroundings = A.Region startCur cur
        region = toRegion cur
      in
      Report.Report "UNFINISHED IF" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I got stuck after the start of this `else` branch:"
          ,
            D.stack
              [ D.reflow $
                  "I was expecting to see an expression next. Maybe it is not filled in yet?"
              , D.toSimpleNote $
                  "I can be confused by indentation, so if the `else` branch is already\
                  \ present, it may not be indented enough for me to recognize it."
              ]
          )

    IfIndentElse cur ->
      case Code.nextLineStartsWithKeyword "else" source cur of
        Code.Yes elseCur ->
          let
            surroundings = A.Region startCur elseCur
            region = toWiderRegion elseCur 4#Word64
          in
          Report.Report "WEIRD ELSE BRANCH" region [] $
            Code.toSnippet source surroundings (Just region)
              (
                D.reflow $
                  "I was partway through an `if` expression when I got stuck here:"
              ,
                D.fillSep $
                  ["I","think","this",D.cyan "else","keyword","needs","to","be","indented","more."
                  ,"Try","adding","some","spaces","before","it."
                  ]
              )

        Code.No ->
          let
            surroundings = A.Region startCur cur
            region = toRegion cur
          in
          Report.Report "UNFINISHED IF" region [] $
            Code.toSnippet source surroundings (Just region)
              (
                D.reflow $
                  "I was expecting to see an `else` branch after this:"
              ,
                D.stack
                  [ D.fillSep
                      ["I","know","what","to","do","when","the","condition","is","True,"
                      ,"but","what","happens","when","it","is","False?"
                      ,"Add","an",D.cyan "else","branch","to","handle","that","scenario!"
                      ]
                  ]
              )



-- RECORD


toRecordReport :: Code.Source -> Context -> Record -> Cursor -> Report.Report
toRecordReport source context record startCur =
  case record of
    RecordOpen cur ->
      case Code.whatIsNext source cur of
        Code.Keyword keyword ->
          let
            surroundings = A.Region startCur cur
            region = toKeywordRegion cur keyword
          in
          Report.Report "RESERVED WORD" region [] $
            Code.toSnippet source surroundings (Just region)
              (
                D.reflow $
                  "I just started parsing a record, but I got stuck on this field name:"
              ,
                D.reflow $
                  "It looks like you are trying to use `" ++ keyword ++ "` as a field name, but \
                  \ that is a reserved word. Try using a different name!"
              )

        _ ->
          let
            surroundings = A.Region startCur cur
            region = toRegion cur
          in
          Report.Report "PROBLEM IN RECORD" region [] $
            Code.toSnippet source surroundings (Just region)
              (
                D.reflow $
                  "I just started parsing a record, but I got stuck here:"
              ,
                D.stack
                  [ D.fillSep
                      ["I","was","expecting","to","see","a","record","field","defined","next,"
                      ,"so","I","am","looking","for","a","name","like"
                      ,D.dullyellow "userName","or",D.dullyellow "plantHeight" <> "."
                      ]
                  , D.toSimpleNote $
                      "Field names must start with a lower-case letter. After that, you can use\
                      \ any sequence of letters, numbers, and underscores."
                  , noteForRecordError
                  ]
              )

    RecordEnd cur ->
      let
        surroundings = A.Region startCur cur
        region = toRegion cur
      in
      Report.Report "PROBLEM IN RECORD" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I am partway through parsing a record, but I got stuck here:"
          ,
            D.stack
              [ D.fillSep
                  ["I","was","expecting","to","see","a","closing","curly","brace","before","this,"
                  ,"so","try","adding","a",D.dullyellow "}","and","see","if","that","helps?"
                  ]
              , D.toSimpleNote $
                  "When I get stuck like this, it usually means that there is a missing parenthesis\
                  \ or bracket somewhere earlier. It could also be a stray keyword or operator."
              ]
          )

    RecordField cur ->
      case Code.whatIsNext source cur of
        Code.Keyword keyword ->
          let
            surroundings = A.Region startCur cur
            region = toKeywordRegion cur keyword
          in
          Report.Report "RESERVED WORD" region [] $
            Code.toSnippet source surroundings (Just region)
              (
                D.reflow $
                  "I am partway through parsing a record, but I got stuck on this field name:"
              ,
                D.reflow $
                  "It looks like you are trying to use `" ++ keyword ++ "` as a field name, but \
                  \ that is a reserved word. Try using a different name!"
              )

        Code.Other (Just ',') ->
          let
            surroundings = A.Region startCur cur
            region = toRegion cur
          in
          Report.Report "EXTRA COMMA" region [] $
            Code.toSnippet source surroundings (Just region)
              (
                D.reflow $
                  "I am partway through parsing a record, but I got stuck here:"
              ,
                D.stack
                  [ D.reflow $
                      "I am seeing two commas in a row. This is the second one!"
                  , D.reflow $
                      "Just delete one of the commas and you should be all set!"
                  , noteForRecordError
                  ]
              )

        Code.Close _ '}' ->
          let
            surroundings = A.Region startCur cur
            region = toRegion cur
          in
          Report.Report "EXTRA COMMA" region [] $
            Code.toSnippet source surroundings (Just region)
              (
                D.reflow $
                  "I am partway through parsing a record, but I got stuck here:"
              ,
                D.stack
                  [ D.reflow $
                      "Trailing commas are not allowed in records. Try deleting the comma that appears\
                      \ before this closing curly brace."
                  , noteForRecordError
                  ]
              )

        _ ->
          let
            surroundings = A.Region startCur cur
            region = toRegion cur
          in
          Report.Report "PROBLEM IN RECORD" region [] $
            Code.toSnippet source surroundings (Just region)
              (
                D.reflow $
                  "I am partway through parsing a record, but I got stuck here:"
              ,
                D.stack
                  [ D.fillSep
                      ["I","was","expecting","to","see","another","record","field","defined","next,"
                      ,"so","I","am","looking","for","a","name","like"
                      ,D.dullyellow "userName","or",D.dullyellow "plantHeight" <> "."
                      ]
                  , D.toSimpleNote $
                      "Field names must start with a lower-case letter. After that, you can use\
                      \ any sequence of letters, numbers, and underscores."
                  , noteForRecordError
                  ]
              )

    RecordEquals cur ->
      let
        surroundings = A.Region startCur cur
        region = toRegion cur
      in
      Report.Report "PROBLEM IN RECORD" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I am partway through parsing a record, but I got stuck here:"
          ,
            D.stack
              [ D.fillSep $
                  ["I","just","saw","a","field","name,","so","I","was","expecting","to","see"
                  ,"an","equals","sign","next.","So","try","putting","an",D.green "=","sign","here?"
                  ]
              , noteForRecordError
              ]
          )

    RecordExpr expr cur ->
      toExprReport source (InNode NRecord startCur context) expr cur

    RecordSpace space cur ->
      toSpaceReport source space cur

    RecordIndentOpen cur ->
      let
        surroundings = A.Region startCur cur
        region = toRegion cur
      in
      Report.Report "UNFINISHED RECORD" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I just saw the opening curly brace of a record, but then I got stuck here:"
          ,
            D.stack
              [ D.fillSep $
                  ["I","am","expecting","a","record","like",D.dullyellow "{ x = 3, y = 4 }","here."
                  ,"Try","defining","some","fields","of","your","own?"
                  ]
              , noteForRecordIndentError
              ]
          )

    RecordIndentEnd cur ->
      case Code.nextLineStartsWithCloseCurly source cur of
        Code.Yes curlyCur ->
          let
            surroundings = A.Region startCur curlyCur
            region = toRegion curlyCur
          in
          Report.Report "NEED MORE INDENTATION" region [] $
            Code.toSnippet source surroundings (Just region)
              (
                D.reflow $
                  "I was partway through parsing a record, but I got stuck here:"
              ,
                D.stack
                  [ D.reflow $
                      "I need this curly brace to be indented more. Try adding some spaces before it!"
                  , noteForRecordError
                  ]
              )

        Code.No ->
          let
            surroundings = A.Region startCur cur
            region = toRegion cur
          in
          Report.Report "UNFINISHED RECORD" region [] $
            Code.toSnippet source surroundings (Just region)
              (
                D.reflow $
                  "I was partway through parsing a record, but I got stuck here:"
              ,
                D.stack
                  [ D.fillSep $
                      ["I","was","expecting","to","see","a","closing","curly","brace","next."
                      ,"Try","putting","a",D.green "}","next","and","see","if","that","helps?"
                      ]
                  , noteForRecordIndentError
                  ]
              )

    RecordIndentField cur ->
      let
        surroundings = A.Region startCur cur
        region = toRegion cur
      in
      Report.Report "UNFINISHED RECORD" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I am partway through parsing a record, but I got stuck after that last comma:"
          ,
            D.stack
              [ D.reflow $
                  "Trailing commas are not allowed in records, so the fix may be to\
                  \ delete that last comma? Or maybe you were in the middle of defining\
                  \ an additional field?"
              , noteForRecordError
              ]
          )

    RecordIndentEquals cur ->
      let
        surroundings = A.Region startCur cur
        region = toRegion cur
      in
      Report.Report "UNFINISHED RECORD" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I am partway through parsing a record. I just saw a record\
              \ field, so I was expecting to see an equals sign next:"
          ,
            D.stack
              [ D.fillSep $
                  ["Try","putting","an",D.green "=","followed","by","an","expression?"
                  ]
              , noteForRecordIndentError
              ]
          )

    RecordIndentExpr cur ->
      let
        surroundings = A.Region startCur cur
        region = toRegion cur
      in
      Report.Report "UNFINISHED RECORD" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I am partway through parsing a record, and I was expecting to run into an expression next:"
          ,
            D.stack
              [ D.fillSep $
                  ["Try","putting","something","like"
                  ,D.dullyellow "42","or",D.dullyellow"\"hello\"","for","now?"
                  ]
              , noteForRecordIndentError
              ]
          )


noteForRecordError :: D.Doc
noteForRecordError =
  D.stack $
    [ D.toSimpleNote
        "If you are trying to define a record across multiple lines, I recommend using this format:"
    , D.indent 4 $ D.vcat $
        [ "{ name = " <> D.dullyellow "\"Alice\""
        , ", age = " <> D.dullyellow "42"
        , ", height = " <> D.dullyellow "1.75"
        , "}"
        ]
    , D.reflow $
        "Notice that each line starts with some indentation. Usually two or four spaces.\
        \ This is the stylistic convention in the Elm ecosystem."
    ]


noteForRecordIndentError :: D.Doc
noteForRecordIndentError =
  D.stack
    [ D.toSimpleNote
        "I may be confused by indentation. For example, if you are trying to define\
        \ a record across multiple lines, I recommend using this format:"
    , D.indent 4 $ D.vcat $
        [ "{ name = " <> D.dullyellow "\"Alice\""
        , ", age = " <> D.dullyellow "42"
        , ", height = " <> D.dullyellow "1.75"
        , "}"
        ]
    , D.reflow $
        "Notice that each line starts with some indentation. Usually two or four spaces.\
        \ This is the stylistic convention in the Elm ecosystem!"
    ]



-- TUPLE


toTupleReport :: Code.Source -> Context -> Tuple -> Cursor -> Report.Report
toTupleReport source context tuple startCur =
  case tuple of
    TupleExpr expr cur ->
      toExprReport source (InNode NParens startCur context) expr cur

    TupleSpace space cur ->
      toSpaceReport source space cur

    TupleEnd cur ->
      let
        surroundings = A.Region startCur cur
        region = toRegion cur
      in
      Report.Report "UNFINISHED PARENTHESES" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I was expecting to see a closing parentheses next, but I got stuck here:"
          ,
            D.stack
              [ D.fillSep ["Try","adding","a",D.dullyellow ")","to","see","if","that","helps?"]
              , D.toSimpleNote $
                  "I can get stuck when I run into keywords, operators, parentheses, or brackets\
                  \ unexpectedly. So there may be some earlier syntax trouble (like extra parenthesis\
                  \ or missing brackets) that is confusing me."
              ]
          )

    TupleOperatorClose cur ->
      let
        surroundings = A.Region startCur cur
        region = toRegion cur
      in
      Report.Report "UNFINISHED OPERATOR FUNCTION" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow "I was expecting a closing parenthesis here:"
          ,
            D.stack
              [ D.fillSep ["Try","adding","a",D.dullyellow ")","to","see","if","that","helps!"]
              , D.toSimpleNote $
                  "I think I am parsing an operator function right now, so I am expecting to see\
                  \ something like (+) or (&&) where an operator is surrounded by parentheses with\
                  \ no extra spaces."
              ]
          )

    TupleOperatorReserved operator cur ->
      let
        surroundings = A.Region startCur cur
        region = toRegion cur
      in
      Report.Report "UNEXPECTED SYMBOL" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I ran into an unexpected symbol here:"
          ,
            D.fillSep $
              case operator of
                BadDot -> ["Maybe","you","wanted","a","record","accessor","like",D.dullyellow ".x","or",D.dullyellow ".name","instead?"]
                BadPipe -> ["Try",D.dullyellow "(||)","instead?","To","turn","boolean","OR","into","a","function?"]
                BadArrow -> ["Maybe","you","wanted",D.dullyellow "(>)","or",D.dullyellow "(>=)","instead?"]
                BadEquals -> ["Try",D.dullyellow "(==)","instead?","To","make","a","function","that","checks","equality?"]
                BadHasType -> ["Try",D.dullyellow "(::)","instead?","To","add","values","to","the","front","of","lists?"]
          )

    TupleIndentExpr1 cur ->
      let
        surroundings = A.Region startCur cur
        region = toRegion cur
      in
      Report.Report "UNFINISHED PARENTHESES" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I just saw an open parenthesis, so I was expecting to see an expression next."
          ,
            D.stack
              [ D.fillSep $
                  ["Something","like",D.dullyellow "(4 + 5)","or"
                  ,D.dullyellow "(String.reverse \"desserts\")" <> "."
                  ,"Anything","where","you","are","putting","parentheses","around","normal","expressions."
                  ]
              , D.toSimpleNote $
                  "I can get confused by indentation in cases like this, so\
                  \ maybe you have an expression but it is not indented enough?"
              ]
          )

    TupleIndentExprN cur ->
      let
        surroundings = A.Region startCur cur
        region = toRegion cur
      in
      Report.Report "UNFINISHED TUPLE" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I think I am in the middle of parsing a tuple. I just saw a comma, so I was expecting to see an expression next."
          ,
            D.stack
              [ D.fillSep $
                  ["A","tuple","looks","like",D.dullyellow "(3,4)","or"
                  ,D.dullyellow "(\"Tom\",42)" <> ","
                  ,"so","I","think","there","is","an","expression","missing","here?"
                  ]
              , D.toSimpleNote $
                  "I can get confused by indentation in cases like this, so\
                  \ maybe you have an expression but it is not indented enough?"
              ]
          )

    TupleIndentEnd cur ->
      let
        surroundings = A.Region startCur cur
        region = toRegion cur
      in
      Report.Report "UNFINISHED PARENTHESES" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I was expecting to see a closing parenthesis next:"
          ,
            D.stack
              [ D.fillSep ["Try","adding","a",D.dullyellow ")","to","see","if","that","helps!"]
              , D.toSimpleNote $
                  "I can get confused by indentation in cases like this, so\
                  \ maybe you have a closing parenthesis but it is not indented enough?"
              ]
          )


toListReport :: Code.Source -> Context -> List -> Cursor -> Report.Report
toListReport source context list startCur =
  case list of
    ListSpace space cur ->
      toSpaceReport source space cur

    ListOpen cur ->
      let
        surroundings = A.Region startCur cur
        region = toRegion cur
      in
      Report.Report "UNFINISHED LIST" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I am partway through parsing a list, but I got stuck here:"
          ,
            D.stack
              [ D.fillSep
                  ["I","was","expecting","to","see","a","closing","square","bracket","before","this,"
                  ,"so","try","adding","a",D.dullyellow "]","and","see","if","that","helps?"
                  ]
              , D.toSimpleNote $
                  "When I get stuck like this, it usually means that there is a missing parenthesis\
                  \ or bracket somewhere earlier. It could also be a stray keyword or operator."
              ]
          )

    ListExpr expr cur ->
      case expr of
        Start c ->
          let
            surroundings = A.Region startCur c
            region = toRegion c
          in
          Report.Report "UNFINISHED LIST" region [] $
            Code.toSnippet source surroundings (Just region)
              (
                D.reflow $
                  "I was expecting to see another list entry after that last comma:"
              ,
                D.stack
                  [ D.reflow $
                      "Trailing commas are not allowed in lists, so the fix may be to delete the comma?"
                  , D.toSimpleNote
                      "I recommend using the following format for lists that span multiple lines:"
                  , D.indent 4 $ D.vcat $
                      [ "[ " <> D.dullyellow "\"Alice\""
                      , ", " <> D.dullyellow "\"Bob\""
                      , ", " <> D.dullyellow "\"Chuck\""
                      , "]"
                      ]
                  , D.reflow $
                      "Notice that each line starts with some indentation. Usually two or four spaces.\
                      \ This is the stylistic convention in the Elm ecosystem."
                  ]
              )

        _ ->
          toExprReport source (InNode NList startCur context) expr cur

    ListEnd cur ->
      let
        surroundings = A.Region startCur cur
        region = toRegion cur
      in
      Report.Report "UNFINISHED LIST" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I am partway through parsing a list, but I got stuck here:"
          ,
            D.stack
              [ D.fillSep
                  ["I","was","expecting","to","see","a","closing","square","bracket","before","this,"
                  ,"so","try","adding","a",D.dullyellow "]","and","see","if","that","helps?"
                  ]
              , D.toSimpleNote $
                  "When I get stuck like this, it usually means that there is a missing parenthesis\
                  \ or bracket somewhere earlier. It could also be a stray keyword or operator."
              ]
          )

    ListIndentOpen cur ->
      let
        surroundings = A.Region startCur cur
        region = toRegion cur
      in
      Report.Report "UNFINISHED LIST" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I cannot find the end of this list:"
          ,
            D.stack
              [ D.fillSep $
                  ["You","could","change","it","to","something","like"
                  ,D.dullyellow "[3,4,5]"
                  ,"or","even","just"
                  ,D.dullyellow "[]" <> "."
                  ,"Anything","where","there","is","an","open","and","close","square","brace,"
                  ,"and","where","the","elements","of","the","list","are","separated","by","commas."
                  ]
              , D.toSimpleNote
                  "I may be confused by indentation. For example, if you are trying to define\
                  \ a list across multiple lines, I recommend using this format:"
              , D.indent 4 $ D.vcat $
                  [ "[ " <> D.dullyellow "\"Alice\""
                  , ", " <> D.dullyellow "\"Bob\""
                  , ", " <> D.dullyellow "\"Chuck\""
                  , "]"
                  ]
              , D.reflow $
                  "Notice that each line starts with some indentation. Usually two or four spaces.\
                  \ This is the stylistic convention in the Elm ecosystem."
              ]
          )

    ListIndentEnd cur ->
      let
        surroundings = A.Region startCur cur
        region = toRegion cur
      in
      Report.Report "UNFINISHED LIST" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I cannot find the end of this list:"
          ,
            D.stack
              [ D.fillSep $
                  ["You","can","just","add","a","closing",D.dullyellow "]"
                  ,"right","here,","and","I","will","be","all","set!"
                  ]
              , D.toSimpleNote
                  "I may be confused by indentation. For example, if you are trying to define\
                  \ a list across multiple lines, I recommend using this format:"
              , D.indent 4 $ D.vcat $
                  [ "[ " <> D.dullyellow "\"Alice\""
                  , ", " <> D.dullyellow "\"Bob\""
                  , ", " <> D.dullyellow "\"Chuck\""
                  , "]"
                  ]
              , D.reflow $
                  "Notice that each line starts with some indentation. Usually two or four spaces.\
                  \ This is the stylistic convention in the Elm ecosystem."
              ]
          )

    ListIndentExpr cur ->
      let
        surroundings = A.Region startCur cur
        region = toRegion cur
      in
      Report.Report "UNFINISHED LIST" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I was expecting to see another list entry after this comma:"
          ,
            D.stack
              [ D.reflow $
                  "Trailing commas are not allowed in lists, so the fix may be to delete the comma?"
              , D.toSimpleNote
                  "I recommend using the following format for lists that span multiple lines:"
              , D.indent 4 $ D.vcat $
                  [ "[ " <> D.dullyellow "\"Alice\""
                  , ", " <> D.dullyellow "\"Bob\""
                  , ", " <> D.dullyellow "\"Chuck\""
                  , "]"
                  ]
              , D.reflow $
                  "Notice that each line starts with some indentation. Usually two or four spaces.\
                  \ This is the stylistic convention in the Elm ecosystem."
              ]
          )


toFuncReport :: Code.Source -> Context -> Func -> Cursor -> Report.Report
toFuncReport source context func startCur =
  case func of
    FuncSpace space cur ->
      toSpaceReport source space cur

    FuncArg pattern cur ->
      toPatternReport source PArg pattern cur

    FuncBody expr cur ->
      toExprReport source (InNode NFunc startCur context) expr cur

    FuncArrow cur ->
      case Code.whatIsNext source cur of
        Code.Keyword keyword ->
          let
            surroundings = A.Region startCur cur
            region = toKeywordRegion cur keyword
          in
          Report.Report "RESERVED WORD" region [] $
            Code.toSnippet source surroundings (Just region)
              (
                D.reflow $
                  "I was parsing an anonymous function, but I got stuck here:"
              ,
                D.reflow $
                  "It looks like you are trying to use `" ++ keyword ++ "` as an argument, but\
                  \ it is a reserved word in this language. Try using a different argument name!"
              )

        _ ->
          let
            surroundings = A.Region startCur cur
            region = toRegion cur
          in
          Report.Report "UNFINISHED ANONYMOUS FUNCTION" region [] $
            Code.toSnippet source surroundings (Just region)
              (
                D.reflow $
                  "I just saw the beginning of an anonymous function, so I was expecting to see an arrow next:"
              ,
                D.fillSep $
                  ["The","syntax","for","anonymous","functions","is"
                  ,D.dullyellow "(\\x -> x + 1)"
                  ,"so","I","am","missing","the","arrow","and","the","body","of","the","function."
                  ]
              )

    FuncIndentArg cur ->
      let
        surroundings = A.Region startCur cur
        region = toRegion cur
      in
      Report.Report "MISSING ARGUMENT" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I just saw the beginning of an anonymous function, so I was expecting to see an argument next:"
          ,
            D.stack
              [ D.fillSep
                  ["Something","like",D.dullyellow"x","or",D.dullyellow "name" <> "."
                  ,"Anything","that","starts","with","a","lower","case","letter!"
                  ]
              , D.toSimpleNote $
                  "The syntax for anonymous functions is (\\x -> x + 1) where the backslash\
                  \ is meant to look a bit like a lambda if you squint. This visual pun seemed\
                  \ like a better idea at the time!"
              ]
          )

    FuncIndentArrow cur ->
      let
        surroundings = A.Region startCur cur
        region = toRegion cur
      in
      Report.Report "UNFINISHED ANONYMOUS FUNCTION" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I just saw the beginning of an anonymous function, so I was expecting to see an arrow next:"
          ,
            D.stack
              [ D.fillSep $
                  ["The","syntax","for","anonymous","functions","is"
                  ,D.dullyellow "(\\x -> x + 1)"
                  ,"so","I","am","missing","the","arrow","and","the","body","of","the","function."
                  ]
              , D.toSimpleNote $
                  "It is possible that I am confused about indetation! I generally recommend\
                  \ switching to named functions if the definition cannot fit inline nicely, so\
                  \ either (1) try to fit the whole anonymous function on one line or (2) break\
                  \ the whole thing out into a named function. Things tend to be clearer that way!"
              ]
          )

    FuncIndentBody cur ->
      let
        surroundings = A.Region startCur cur
        region = toRegion cur
      in
      Report.Report "UNFINISHED ANONYMOUS FUNCTION" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I was expecting to see the body of your anonymous function next:"
          ,
            D.stack
              [ D.fillSep $
                  ["The","syntax","for","anonymous","functions","is"
                  ,D.dullyellow "(\\x -> x + 1)"
                  ,"so","I","am","missing","all","the","stuff","after","the","arrow!"
                  ]
              , D.toSimpleNote $
                  "It is possible that I am confused about indetation! I generally recommend\
                  \ switching to named functions if the definition cannot fit inline nicely, so\
                  \ either (1) try to fit the whole anonymous function on one line or (2) break\
                  \ the whole thing out into a named function. Things tend to be clearer that way!"
              ]
          )



-- PATTERN


data PContext
  = PCase
  | PArg
  | PLet


toPatternReport :: Code.Source -> PContext -> Pattern -> Cursor -> Report.Report
toPatternReport source context pattern startCur =
  case pattern of
    PRecord record cur ->
      toPRecordReport source record cur

    PTuple tuple cur ->
      toPTupleReport source context tuple cur

    PList list cur ->
      toPListReport source context list cur

    PStart cur ->
      case Code.whatIsNext source cur of
        Code.Keyword keyword ->
          let
            surroundings = A.Region startCur cur
            region = toKeywordRegion cur keyword
            inThisThing =
              case context of
                PArg  -> "as an argument"
                PCase -> "in this pattern"
                PLet  -> "in this pattern"
          in
          Report.Report "RESERVED WORD" region [] $
            Code.toSnippet source surroundings (Just region)
              (
                D.reflow $
                  "It looks like you are trying to use `" ++ keyword ++ "` " ++ inThisThing ++ ":"
              ,
                D.reflow $
                  "This is a reserved word! Try using some other name?"
              )

        Code.Operator "-" ->
          let
            surroundings = A.Region startCur cur
            region = toRegion cur
          in
          Report.Report "UNEXPECTED SYMBOL" region [] $
            Code.toSnippet source surroundings (Just region)
              (
                D.reflow $
                  "I ran into a minus sign unexpectedly in this pattern:"
              ,
                D.reflow $
                  "It is not possible to pattern match on negative numbers at this\
                  \ time. Try using an `if` expression for that sort of thing for now."
              )

        _ ->
          let
            surroundings = A.Region startCur cur
            region = toRegion cur
          in
          Report.Report "PROBLEM IN PATTERN" region [] $
            Code.toSnippet source surroundings (Just region)
              (
                D.reflow $
                  "I wanted to parse a pattern next, but I got stuck here:"
              ,
                D.fillSep $
                  ["I","am","not","sure","why","I","am","getting","stuck","exactly."
                  ,"I","just","know","that","I","want","a","pattern","next."
                  ,"Something","as","simple","as"
                  ,D.dullyellow "maybeHeight","or",D.dullyellow "result"
                  ,"would","work!"
                  ]
              )

    PChar char cur ->
      toCharReport source char cur

    PString string cur ->
      toStringReport source string cur

    PNumber number cur ->
      toNumberReport source number cur

    PFloat width cur ->
      let
        region = toWiderRegion cur width
      in
      Report.Report "UNEXPECTED PATTERN" region [] $
        Code.toSnippet source region Nothing
          (
            D.reflow $
              "I cannot pattern match with floating point numbers:"
          ,
            D.fillSep $
              ["Equality","on","floats","can","be","unreliable,","so","you","usually","want"
              ,"to","check","that","they","are","nearby","with","some","sort","of"
              ,D.dullyellow "(abs (actual - expected) < 0.001)","check."
              ]
          )

    PAlias cur ->
      let
        region = toRegion cur
      in
      Report.Report "UNFINISHED PATTERN" region [] $
        Code.toSnippet source region Nothing $
          (
            D.reflow $
              "I was expecting to see a variable name after the `as` keyword:"
          ,
            D.stack
              [ D.fillSep $
                  ["The","`as`","keyword","lets","you","write","patterns","like"
                  ,"((" <> D.dullyellow "x" <> "," <> D.dullyellow "y" <> ") " <> D.cyan "as" <> D.dullyellow " point" <> ")"
                  ,"so","you","can","refer","to","individual","parts","of","the","tuple","with"
                  ,D.dullyellow "x","and",D.dullyellow "y","or","you","refer","to","the","whole"
                  ,"thing","with",D.dullyellow "point" <> "."
                  ]
              , D.reflow $
                  "So I was expecting to see a variable name after the `as` keyword here. Sometimes\
                  \ people just want to use `as` as a variable name though. Try using a different name\
                  \ in that case!"
              ]
          )

    PWildcardNotVar name width cur ->
      let
        region = toWiderRegion cur width
        examples =
          case dropWhile (=='_') (Name.toChars name) of
            [] -> [D.dullyellow "x","or",D.dullyellow "age"]
            c:cs -> [D.dullyellow (D.fromChars (Char.toLower c : cs))]
      in
      Report.Report "UNEXPECTED NAME" region [] $
        Code.toSnippet source region Nothing $
          (
            D.reflow $
              "Variable names cannot start with underscores like this:"
          ,
            D.fillSep $
              ["You","can","either","have","an","underscore","like",D.dullyellow "_","to"
              ,"ignore","the","value,","or","you","can","have","a","name","like"
              ] ++ examples ++ ["to","use","the","matched","value." ]
          )

    PSpace space cur ->
      toSpaceReport source space cur

    PIndentStart cur ->
      let
        surroundings = A.Region startCur cur
        region = toRegion cur
      in
      Report.Report "UNFINISHED PATTERN" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I wanted to parse a pattern next, but I got stuck here:"
          ,
            D.stack
              [ D.fillSep $
                  ["I","am","not","sure","why","I","am","getting","stuck","exactly."
                  ,"I","just","know","that","I","want","a","pattern","next."
                  ,"Something","as","simple","as"
                  ,D.dullyellow "maybeHeight","or",D.dullyellow "result"
                  ,"would","work!"
                  ]
              , D.toSimpleNote $
                  "I can get confused by indentation. If you think there is a pattern next, maybe\
                  \ it needs to be indented a bit more?"
              ]
          )

    PIndentAlias cur ->
      let
        region = toRegion cur
      in
      Report.Report "UNFINISHED PATTERN" region [] $
        Code.toSnippet source region Nothing $
          (
            D.reflow $
              "I was expecting to see a variable name after the `as` keyword:"
          ,
            D.stack
              [ D.fillSep $
                  ["The","`as`","keyword","lets","you","write","patterns","like"
                  ,"((" <> D.dullyellow "x" <> "," <> D.dullyellow "y" <> ") " <> D.cyan "as" <> D.dullyellow " point" <> ")"
                  ,"so","you","can","refer","to","individual","parts","of","the","tuple","with"
                  ,D.dullyellow "x","and",D.dullyellow "y","or","you","refer","to","the","whole"
                  ,"thing","with",D.dullyellow "point."
                  ]
              , D.reflow $
                  "So I was expecting to see a variable name after the `as` keyword here. Sometimes\
                  \ people just want to use `as` as a variable name though. Try using a different name\
                  \ in that case!"
              ]
          )


toPRecordReport :: Code.Source -> PRecord -> Cursor -> Report.Report
toPRecordReport source record startCur =
  case record of
    PRecordOpen cur ->
      toUnfinishRecordPatternReport source cur startCur $
        D.reflow "I was expecting to see a field name next."

    PRecordEnd cur ->
      toUnfinishRecordPatternReport source cur startCur $
        D.fillSep
          ["I","was","expecting","to","see","a","closing","curly","brace","next."
          ,"Try","adding","a",D.dullyellow "}","here?"
          ]

    PRecordField cur ->
      case Code.whatIsNext source cur of
        Code.Keyword keyword ->
          let
            surroundings = A.Region startCur cur
            region = toKeywordRegion cur keyword
          in
          Report.Report "RESERVED WORD" region [] $
            Code.toSnippet source surroundings (Just region)
              (
                D.reflow $
                  "I was not expecting to see `" ++ keyword ++ "` as a record field name:"
              ,
                D.reflow $
                  "This is a reserved word, not available for variable names. Try another name!"
              )

        _ ->
          toUnfinishRecordPatternReport source cur startCur $
            D.reflow "I was expecting to see a field name next."

    PRecordSpace space cur ->
      toSpaceReport source space cur

    PRecordIndentOpen cur ->
      toUnfinishRecordPatternReport source cur startCur $
        D.reflow "I was expecting to see a field name next."

    PRecordIndentEnd cur ->
      toUnfinishRecordPatternReport source cur startCur $
        D.fillSep
          ["I","was","expecting","to","see","a","closing","curly","brace","next."
          ,"Try","adding","a",D.dullyellow "}","here?"
          ]

    PRecordIndentField cur ->
      toUnfinishRecordPatternReport source cur startCur $
        D.reflow "I was expecting to see a field name next."


toUnfinishRecordPatternReport :: Code.Source -> Cursor -> Cursor -> D.Doc -> Report.Report
toUnfinishRecordPatternReport source cur startCur message =
  let
    surroundings = A.Region startCur cur
    region = toRegion cur
  in
  Report.Report "UNFINISHED RECORD PATTERN" region [] $
    Code.toSnippet source surroundings (Just region)
      (
        D.reflow $
          "I was partway through parsing a record pattern, but I got stuck here:"
      ,
        D.stack
          [ message
          , D.toFancyHint $
              ["A","record","pattern","looks","like",D.dullyellow "{x,y}","or",D.dullyellow "{name,age}"
              ,"where","you","list","the","field","names","you","want","to","access."
              ]
          ]
      )



toPTupleReport :: Code.Source -> PContext -> PTuple -> Cursor -> Report.Report
toPTupleReport source context tuple startCur =
  case tuple of
    PTupleOpen cur ->
      case Code.whatIsNext source cur of
        Code.Keyword keyword ->
          let
            surroundings = A.Region startCur cur
            region = toKeywordRegion cur keyword
          in
          Report.Report "RESERVED WORD" region [] $
            Code.toSnippet source surroundings (Just region)
              (
                D.reflow $
                  "It looks like you are trying to use `" ++ keyword ++ "` as a variable name:"
              ,
                D.reflow $
                  "This is a reserved word! Try using some other name?"
              )

        _ ->
          let
            surroundings = A.Region startCur cur
            region = toRegion cur
          in
          Report.Report "UNFINISHED PARENTHESES" region [] $
            Code.toSnippet source surroundings (Just region)
              (
                D.reflow $
                  "I just saw an open parenthesis, but I got stuck here:"
              ,
                D.fillSep
                  ["I","was","expecting","to","see","a","pattern","next."
                  ,"Maybe","it","will","end","up","being","something"
                  ,"like",D.dullyellow "(x,y)","or",D.dullyellow "(name, _)" <> "?"
                  ]
              )

    PTupleEnd cur ->
      case Code.whatIsNext source cur of
        Code.Keyword keyword ->
          let
            surroundings = A.Region startCur cur
            region = toKeywordRegion cur keyword
          in
          Report.Report "RESERVED WORD" region [] $
            Code.toSnippet source surroundings (Just region) $
              (
                D.reflow $
                  "I ran into a reserved word in this pattern:"
              ,
                D.reflow $
                  "The `" ++ keyword ++ "` keyword is reserved. Try using a different name instead!"
              )

        Code.Operator op ->
          let
            surroundings = A.Region startCur cur
            region = toKeywordRegion cur op
          in
          Report.Report "UNEXPECTED SYMBOL" region [] $
            Code.toSnippet source surroundings (Just region) $
              (
                D.reflow $
                  "I ran into the " ++ op ++ " symbol unexpectedly in this pattern:"
              ,
                D.reflow $
                  "Only the :: symbol that works in patterns. It is useful if you\
                  \ are pattern matching on lists, trying to get the first element\
                  \ off the front. Did you want that instead?"
              )

        Code.Close term bracket ->
          let
            surroundings = A.Region startCur cur
            region = toRegion cur
          in
          Report.Report ("STRAY " ++ map Char.toUpper term) region [] $
            Code.toSnippet source surroundings (Just region) $
              (
                D.reflow $
                  "I ran into a an unexpected " ++ term ++ " in this pattern:"
              ,
                D.reflow $
                  "This " ++ bracket : " does not match up with an earlier open " ++ term ++ ". Try deleting it?"
              )

        _ ->
          let
            surroundings = A.Region startCur cur
            region = toRegion cur
          in
          Report.Report "UNFINISHED PARENTHESES" region [] $
            Code.toSnippet source surroundings (Just region) $
              (
                D.reflow $
                  "I was partway through parsing a pattern, but I got stuck here:"
              ,
                D.fillSep
                  ["I","was","expecting","a","closing","parenthesis","next,","so"
                  ,"try","adding","a",D.dullyellow ")","to","see","if","that","helps?"
                  ]
              )

    PTupleExpr pattern cur ->
      toPatternReport source context pattern cur

    PTupleSpace space cur ->
      toSpaceReport source space cur

    PTupleIndentEnd cur ->
      let
        surroundings = A.Region startCur cur
        region = toRegion cur
      in
      Report.Report "UNFINISHED PARENTHESES" region [] $
        Code.toSnippet source surroundings (Just region) $
          (
            D.reflow $
              "I was expecting a closing parenthesis next:"
          ,
            D.stack
              [ D.fillSep ["Try","adding","a",D.dullyellow ")","to","see","if","that","helps?"]
              , D.toSimpleNote $
                  "I can get confused by indentation in cases like this, so\
                  \ maybe you have a closing parenthesis but it is not indented enough?"
              ]
          )

    PTupleIndentExpr1 cur ->
      let
        surroundings = A.Region startCur cur
        region = toRegion cur
      in
      Report.Report "UNFINISHED PARENTHESES" region [] $
        Code.toSnippet source surroundings (Just region) $
          (
            D.reflow $
              "I just saw an open parenthesis, but then I got stuck here:"
          ,
            D.fillSep
              ["I","was","expecting","to","see","a","pattern","next."
              ,"Maybe","it","will","end","up","being","something"
              ,"like",D.dullyellow "(x,y)","or",D.dullyellow "(name, _)" <> "?"
              ]
          )

    PTupleIndentExprN cur ->
      let
        surroundings = A.Region startCur cur
        region = toRegion cur
      in
      Report.Report "UNFINISHED TUPLE PATTERN" region [] $
        Code.toSnippet source surroundings (Just region) $
          (
            D.reflow $
              "I am partway through parsing a tuple pattern, but I got stuck here:"
          ,
            D.stack
              [ D.fillSep
                  ["I","was","expecting","to","see","a","pattern","next."
                  ,"I","am","expecting","the","final","result","to","be","something"
                  ,"like",D.dullyellow "(x,y)","or",D.dullyellow "(name, _)" <> "."
                  ]
              , D.toSimpleNote $
                  "I can get confused by indentation in cases like this, so the problem\
                  \ may be that the next part is not indented enough?"
              ]
          )


toPListReport :: Code.Source -> PContext -> PList -> Cursor -> Report.Report
toPListReport source context list startCur =
  case list of
    PListOpen cur ->
      case Code.whatIsNext source cur of
        Code.Keyword keyword ->
          let
            surroundings = A.Region startCur cur
            region = toKeywordRegion cur keyword
          in
          Report.Report "RESERVED WORD" region [] $
            Code.toSnippet source surroundings (Just region)
              (
                D.reflow $
                  "It looks like you are trying to use `" ++ keyword ++ "` to name an element of a list:"
              ,
                D.reflow $
                  "This is a reserved word though! Try using some other name?"
              )

        _ ->
          let
            surroundings = A.Region startCur cur
            region = toRegion cur
          in
          Report.Report "UNFINISHED LIST PATTERN" region [] $
            Code.toSnippet source surroundings (Just region) $
              (
                D.reflow $
                  "I just saw an open square bracket, but then I got stuck here:"
              ,
                D.fillSep ["Try","adding","a",D.dullyellow "]","to","see","if","that","helps?"]
              )

    PListEnd cur ->
      let
        surroundings = A.Region startCur cur
        region = toRegion cur
      in
      Report.Report "UNFINISHED LIST PATTERN" region [] $
        Code.toSnippet source surroundings (Just region) $
          (
            D.reflow $
              "I was expecting a closing square bracket to end this list pattern:"
          ,
            D.fillSep ["Try","adding","a",D.dullyellow "]","to","see","if","that","helps?"]
          )

    PListExpr pattern cur ->
      toPatternReport source context pattern cur

    PListSpace space cur ->
      toSpaceReport source space cur

    PListIndentOpen cur ->
      let
        surroundings = A.Region startCur cur
        region = toRegion cur
      in
      Report.Report "UNFINISHED LIST PATTERN" region [] $
        Code.toSnippet source surroundings (Just region) $
          (
            D.reflow $
              "I just saw an open square bracket, but then I got stuck here:"
          ,
            D.stack
              [ D.fillSep ["Try","adding","a",D.dullyellow "]","to","see","if","that","helps?"]
              , D.toSimpleNote $
                  "I can get confused by indentation in cases like this, so\
                  \ maybe there is something next, but it is not indented enough?"
              ]
          )

    PListIndentEnd cur ->
      let
        surroundings = A.Region startCur cur
        region = toRegion cur
      in
      Report.Report "UNFINISHED LIST PATTERN" region [] $
        Code.toSnippet source surroundings (Just region) $
          (
            D.reflow $
              "I was expecting a closing square bracket to end this list pattern:"
          ,
            D.stack
              [ D.fillSep ["Try","adding","a",D.dullyellow "]","to","see","if","that","helps?"]
              , D.toSimpleNote $
                  "I can get confused by indentation in cases like this, so\
                  \ maybe you have a closing square bracket but it is not indented enough?"
              ]
          )

    PListIndentExpr cur ->
      let
        surroundings = A.Region startCur cur
        region = toRegion cur
      in
      Report.Report "UNFINISHED LIST PATTERN" region [] $
        Code.toSnippet source surroundings (Just region) $
          (
            D.reflow $
              "I am partway through parsing a list pattern, but I got stuck here:"
          ,
            D.stack
              [ D.reflow $
                  "I was expecting to see another pattern next. Maybe a variable name."
              , D.toSimpleNote $
                  "I can get confused by indentation in cases like this, so\
                  \ maybe there is more to this pattern but it is not indented enough?"
              ]
          )



-- TYPES


data TContext
  = TC_Annotation Name.Name
  | TC_CustomType
  | TC_TypeAlias
  | TC_Port


toTypeReport :: Code.Source -> TContext -> Type -> Cursor -> Report.Report
toTypeReport source context tipe startCur =
  case tipe of
    TRecord record cur ->
      toTRecordReport source context record cur

    TTuple tuple cur ->
      toTTupleReport source context tuple cur

    TStart cur ->
      case Code.whatIsNext source cur of
        Code.Keyword keyword ->
          let
            surroundings = A.Region startCur cur
            region = toKeywordRegion cur keyword
          in
          Report.Report "RESERVED WORD" region [] $
            Code.toSnippet source surroundings (Just region)
              (
                D.reflow $
                  "I was expecting to see a type next, but I got stuck on this reserved word:"
              ,
                D.reflow $
                  "It looks like you are trying to use `" ++ keyword ++ "` as a type variable, but \
                  \ it is a reserved word. Try using a different name!"
              )

        _ ->
          let
            surroundings = A.Region startCur cur
            region = toRegion cur

            thing =
              case context of
                TC_Annotation _ -> "type annotation"
                TC_CustomType -> "custom type"
                TC_TypeAlias -> "type alias"
                TC_Port -> "port"

            something =
              case context of
                TC_Annotation name -> "the `" ++ Name.toChars name ++ "` type annotation"
                TC_CustomType -> "a custom type"
                TC_TypeAlias -> "a type alias"
                TC_Port -> "a port"
          in
          Report.Report ("PROBLEM IN " ++ map Char.toUpper thing) region [] $
            Code.toSnippet source surroundings (Just region)
              (
                D.reflow $
                  "I was partway through parsing " ++ something ++ ", but I got stuck here:"
              ,
                D.fillSep $
                  ["I","was","expecting","to","see","a","type","next."
                  ,"Try","putting",D.dullyellow "Int","or",D.dullyellow "String","for","now?"
                  ]
              )

    TSpace space cur ->
      toSpaceReport source space cur

    TIndentStart cur ->
      let
        surroundings = A.Region startCur cur
        region = toRegion cur

        thing =
          case context of
            TC_Annotation _ -> "type annotation"
            TC_CustomType -> "custom type"
            TC_TypeAlias -> "type alias"
            TC_Port -> "port"
      in
      Report.Report ("UNFINISHED " ++ map Char.toUpper thing) region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I was partway through parsing a " ++ thing ++ ", but I got stuck here:"
          ,
            D.stack
              [ D.fillSep $
                  ["I","was","expecting","to","see","a","type","next."
                  ,"Try","putting",D.dullyellow "Int","or",D.dullyellow "String","for","now?"
                  ]
              , D.toSimpleNote $
                  "I can get confused by indentation. If you think there is already a type\
                  \ next, maybe it is not indented enough?"
              ]
          )


toTRecordReport :: Code.Source -> TContext -> TRecord -> Cursor -> Report.Report
toTRecordReport source context record startCur =
  case record of
    TRecordOpen cur ->
      case Code.whatIsNext source cur of
        Code.Keyword keyword ->
          let
            surroundings = A.Region startCur cur
            region = toKeywordRegion cur keyword
          in
          Report.Report "RESERVED WORD" region [] $
            Code.toSnippet source surroundings (Just region)
              (
                D.reflow $
                  "I just started parsing a record type, but I got stuck on this field name:"
              ,
                D.reflow $
                  "It looks like you are trying to use `" ++ keyword ++ "` as a field name, but \
                  \ that is a reserved word. Try using a different name!"
              )

        _ ->
          let
            surroundings = A.Region startCur cur
            region = toRegion cur
          in
          Report.Report "UNFINISHED RECORD TYPE" region [] $
            Code.toSnippet source surroundings (Just region)
              (
                D.reflow $
                  "I just started parsing a record type, but I got stuck here:"
              ,
                D.fillSep
                  ["Record","types","look","like",D.dullyellow "{ name : String, age : Int },"
                  ,"so","I","was","expecting","to","see","a","field","name","next."
                  ]
              )

    TRecordEnd cur ->
      let
        surroundings = A.Region startCur cur
        region = toRegion cur
      in
      Report.Report "UNFINISHED RECORD TYPE" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I am partway through parsing a record type, but I got stuck here:"
          ,
            D.stack
              [ D.fillSep
                  ["I","was","expecting","to","see","a","closing","curly","brace","before","this,"
                  ,"so","try","adding","a",D.dullyellow "}","and","see","if","that","helps?"
                  ]
              , D.toSimpleNote $
                  "When I get stuck like this, it usually means that there is a missing parenthesis\
                  \ or bracket somewhere earlier. It could also be a stray keyword or operator."
              ]
          )

    TRecordField cur ->
      case Code.whatIsNext source cur of
        Code.Keyword keyword ->
          let
            surroundings = A.Region startCur cur
            region = toKeywordRegion cur keyword
          in
          Report.Report "RESERVED WORD" region [] $
            Code.toSnippet source surroundings (Just region)
              (
                D.reflow $
                  "I am partway through parsing a record type, but I got stuck on this field name:"
              ,
                D.reflow $
                  "It looks like you are trying to use `" ++ keyword ++ "` as a field name, but \
                  \ that is a reserved word. Try using a different name!"
              )

        Code.Other (Just ',') ->
          let
            surroundings = A.Region startCur cur
            region = toRegion cur
          in
          Report.Report "EXTRA COMMA" region [] $
            Code.toSnippet source surroundings (Just region)
              (
                D.reflow $
                  "I am partway through parsing a record type, but I got stuck here:"
              ,
                D.stack
                  [ D.reflow $
                      "I am seeing two commas in a row. This is the second one!"
                  , D.reflow $
                      "Just delete one of the commas and you should be all set!"
                  , noteForRecordTypeError
                  ]
              )

        Code.Close _ '}' ->
          let
            surroundings = A.Region startCur cur
            region = toRegion cur
          in
          Report.Report "EXTRA COMMA" region [] $
            Code.toSnippet source surroundings (Just region)
              (
                D.reflow $
                  "I am partway through parsing a record type, but I got stuck here:"
              ,
                D.stack
                  [ D.reflow $
                      "Trailing commas are not allowed in record types. Try deleting the comma that\
                      \ appears before this closing curly brace."
                  , noteForRecordTypeError
                  ]
              )

        _ ->
          let
            surroundings = A.Region startCur cur
            region = toRegion cur
          in
          Report.Report "PROBLEM IN RECORD TYPE" region [] $
            Code.toSnippet source surroundings (Just region)
              (
                D.reflow $
                  "I am partway through parsing a record type, but I got stuck here:"
              ,
                D.stack
                  [ D.fillSep
                      ["I","was","expecting","to","see","another","record","field","defined","next,"
                      ,"so","I","am","looking","for","a","name","like"
                      ,D.dullyellow "userName","or",D.dullyellow "plantHeight" <> "."
                      ]
                  , noteForRecordTypeError
                  ]
              )

    TRecordColon cur ->
      let
        surroundings = A.Region startCur cur
        region = toRegion cur
      in
      Report.Report "UNFINISHED RECORD TYPE" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I am partway through parsing a record type, but I got stuck here:"
          ,
            D.stack
              [ D.fillSep $
                  ["I","just","saw","a","field","name,","so","I","was","expecting","to","see"
                  ,"a","colon","next.","So","try","putting","an",D.green ":","sign","here?"
                  ]
              , noteForRecordTypeError
              ]
          )

    TRecordType tipe cur ->
      toTypeReport source context tipe cur

    TRecordSpace space cur ->
      toSpaceReport source space cur

    TRecordIndentOpen cur ->
      let
        surroundings = A.Region startCur cur
        region = toRegion cur
      in
      Report.Report "UNFINISHED RECORD TYPE" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I just saw the opening curly brace of a record type, but then I got stuck here:"
          ,
            D.stack
              [ D.fillSep $
                  ["I","am","expecting","a","record","like",D.dullyellow "{ name : String, age : Int }","here."
                  ,"Try","defining","some","fields","of","your","own?"
                  ]
              , noteForRecordTypeIndentError
              ]
          )

    TRecordIndentEnd cur ->
      case Code.nextLineStartsWithCloseCurly source cur of
        Code.Yes curlyCur ->
          let
            surroundings = A.Region startCur curlyCur
            region = toRegion curlyCur
          in
          Report.Report "NEED MORE INDENTATION" region [] $
            Code.toSnippet source surroundings (Just region)
              (
                D.reflow $
                  "I was partway through parsing a record type, but I got stuck here:"
              ,
                D.stack
                  [ D.reflow $
                      "I need this curly brace to be indented more. Try adding some spaces before it!"
                  , noteForRecordTypeError
                  ]
              )

        Code.No ->
          let
            surroundings = A.Region startCur cur
            region = toRegion cur
          in
          Report.Report "UNFINISHED RECORD TYPE" region [] $
            Code.toSnippet source surroundings (Just region)
              (
                D.reflow $
                  "I was partway through parsing a record type, but I got stuck here:"
              ,
                D.stack
                  [ D.fillSep $
                      ["I","was","expecting","to","see","a","closing","curly","brace","next."
                      ,"Try","putting","a",D.green "}","next","and","see","if","that","helps?"
                      ]
                  , noteForRecordTypeIndentError
                  ]
              )

    TRecordIndentField cur ->
      let
        surroundings = A.Region startCur cur
        region = toRegion cur
      in
      Report.Report "UNFINISHED RECORD TYPE" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I am partway through parsing a record type, but I got stuck after that last comma:"
          ,
            D.stack
              [ D.reflow $
                  "Trailing commas are not allowed in record types, so the fix may be to\
                  \ delete that last comma? Or maybe you were in the middle of defining\
                  \ an additional field?"
              , noteForRecordTypeIndentError
              ]
          )

    TRecordIndentColon cur ->
      let
        surroundings = A.Region startCur cur
        region = toRegion cur
      in
      Report.Report "UNFINISHED RECORD TYPE" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I am partway through parsing a record type. I just saw a record\
              \ field, so I was expecting to see a colon next:"
          ,
            D.stack
              [ D.fillSep $
                  ["Try","putting","an",D.green ":","followed","by","a","type?"
                  ]
              , noteForRecordTypeIndentError
              ]
          )

    TRecordIndentType cur ->
      let
        surroundings = A.Region startCur cur
        region = toRegion cur
      in
      Report.Report "UNFINISHED RECORD TYPE" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I am partway through parsing a record type, and I was expecting to run into a type next:"
          ,
            D.stack
              [ D.fillSep $
                  ["Try","putting","something","like"
                  ,D.dullyellow "Int","or",D.dullyellow "String","for","now?"
                  ]
              , noteForRecordTypeIndentError
              ]
          )


noteForRecordTypeError :: D.Doc
noteForRecordTypeError =
  D.stack $
    [ D.toSimpleNote
        "If you are trying to define a record type across multiple lines, I recommend using this format:"
    , D.indent 4 $ D.vcat $
        [ "{ name : String"
        , ", age : Int"
        , ", height : Float"
        , "}"
        ]
    , D.reflow $
        "Notice that each line starts with some indentation. Usually two or four spaces.\
        \ This is the stylistic convention in the Elm ecosystem."
    ]


noteForRecordTypeIndentError :: D.Doc
noteForRecordTypeIndentError =
  D.stack $
    [ D.toSimpleNote
        "I may be confused by indentation. For example, if you are trying to define\
        \ a record type across multiple lines, I recommend using this format:"
    , D.indent 4 $ D.vcat $
        [ "{ name : String"
        , ", age : Int"
        , ", height : Float"
        , "}"
        ]
    , D.reflow $
        "Notice that each line starts with some indentation. Usually two or four spaces.\
        \ This is the stylistic convention in the Elm ecosystem."
    ]


toTTupleReport :: Code.Source -> TContext -> TTuple -> Cursor -> Report.Report
toTTupleReport source context tuple startCur =
  case tuple of
    TTupleOpen cur ->
      case Code.whatIsNext source cur of
        Code.Keyword keyword ->
          let
            surroundings = A.Region startCur cur
            region = toKeywordRegion cur keyword
          in
          Report.Report "RESERVED WORD" region [] $
            Code.toSnippet source surroundings (Just region)
              (
                D.reflow $
                  "I ran into a reserved word unexpectedly:"
              ,
                D.reflow $
                  "It looks like you are trying to use `" ++ keyword ++ "` as a variable name, but \
                  \ it is a reserved word. Try using a different name!"
              )

        _ ->
          let
            surroundings = A.Region startCur cur
            region = toRegion cur
          in
          Report.Report "UNFINISHED PARENTHESES" region [] $
            Code.toSnippet source surroundings (Just region)
              (
                D.reflow $
                  "I just saw an open parenthesis, so I was expecting to see a type next."
              ,
                D.fillSep $
                  ["Something","like",D.dullyellow "(Maybe Int)","or"
                  ,D.dullyellow "(List Person)" <> "."
                  ,"Anything","where","you","are","putting","parentheses","around","normal","types."
                  ]
              )

    TTupleEnd cur ->
      let
        surroundings = A.Region startCur cur
        region = toRegion cur
      in
      Report.Report "UNFINISHED PARENTHESES" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I was expecting to see a closing parenthesis next, but I got stuck here:"
          ,
            D.stack
              [ D.fillSep ["Try","adding","a",D.dullyellow ")","to","see","if","that","helps?"]
              , D.toSimpleNote $
                  "I can get stuck when I run into keywords, operators, parentheses, or brackets\
                  \ unexpectedly. So there may be some earlier syntax trouble (like extra parenthesis\
                  \ or missing brackets) that is confusing me."
              ]
          )

    TTupleType tipe cur ->
      toTypeReport source context tipe cur

    TTupleSpace space cur ->
      toSpaceReport source space cur

    TTupleIndentType1 cur ->
      let
        surroundings = A.Region startCur cur
        region = toRegion cur
      in
      Report.Report "UNFINISHED PARENTHESES" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I just saw an open parenthesis, so I was expecting to see a type next."
          ,
            D.stack
              [ D.fillSep $
                  ["Something","like",D.dullyellow "(Maybe Int)","or"
                  ,D.dullyellow "(List Person)" <> "."
                  ,"Anything","where","you","are","putting","parentheses","around","normal","types."
                  ]
              , D.toSimpleNote $
                  "I can get confused by indentation in cases like this, so\
                  \ maybe you have a type but it is not indented enough?"
              ]
          )

    TTupleIndentTypeN cur ->
      let
        surroundings = A.Region startCur cur
        region = toRegion cur
      in
      Report.Report "UNFINISHED TUPLE TYPE" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I think I am in the middle of parsing a tuple type. I just saw a comma, so I was expecting to see a type next."
          ,
            D.stack
              [ D.fillSep $
                  ["A","tuple","type","looks","like",D.dullyellow "(Float,Float)","or"
                  ,D.dullyellow "(String,Int)" <> ","
                  ,"so","I","think","there","is","a","type","missing","here?"
                  ]
              , D.toSimpleNote $
                  "I can get confused by indentation in cases like this, so\
                  \ maybe you have an expression but it is not indented enough?"
              ]
          )

    TTupleIndentEnd cur ->
      let
        surroundings = A.Region startCur cur
        region = toRegion cur
      in
      Report.Report "UNFINISHED PARENTHESES" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I was expecting to see a closing parenthesis next:"
          ,
            D.stack
              [ D.fillSep ["Try","adding","a",D.dullyellow ")","to","see","if","that","helps!"]
              , D.toSimpleNote $
                  "I can get confused by indentation in cases like this, so\
                  \ maybe you have a closing parenthesis but it is not indented enough?"
              ]
          )
