{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Reporting.Error.Syntax
  ( Error(..)
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
  , toReport
  )
  where


import Prelude hiding (Char, String)
import qualified Data.Char as Char
import qualified Data.Name as Name
import Data.Word (Word16)
import Numeric (showHex)

import qualified Elm.ModuleName as ModuleName
import Parse.Primitives (Row, Col)
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
  | ParseError Module



-- MODULE


data Module
  = ModuleSpace Space Row Col
  | ModuleEndOfFile Row Col
  --
  | ModuleProblem Row Col
  | ModuleName Row Col
  | ModuleExposing Exposing Row Col
  --
  | PortModuleProblem Row Col
  | PortModuleName Row Col
  | PortModuleExposing Exposing Row Col
  --
  | Effect Row Col
  --
  | FreshLine Row Col
  --
  | ImportStart Row Col
  | ImportName Row Col
  | ImportAs Row Col
  | ImportAlias Row Col
  | ImportExposing Row Col
  | ImportExposingList Exposing Row Col
  | ImportEnd Row Col -- different based on col=1 or if greater
  --
  | ImportIndentName Row Col
  | ImportIndentAlias Row Col
  | ImportIndentExposingList Row Col
  --
  | Infix Row Col
  --
  | Declarations Decl Row Col


data Exposing
  = ExposingSpace Space Row Col
  | ExposingStart Row Col
  | ExposingValue Row Col
  | ExposingOperator Row Col
  | ExposingOperatorReserved BadOperator Row Col
  | ExposingOperatorRightParen Row Col
  | ExposingEnd Row Col
  --
  | ExposingTypePrivacy Row Col
  | ExposingTypePrivacyDots Row Col
  | ExposingTypePrivacyEnd Row Col
  --
  | ExposingIndentEnd Row Col
  | ExposingIndentValue Row Col
  | ExposingIndentValueEnd Row Col
  | ExposingIndentTypePrivacy Row Col
  | ExposingIndentTypePrivacyDots Row Col
  | ExposingIndentTypePrivacyEnd Row Col



-- DECLARATIONS


data Decl
  = DeclStart Row Col
  | DeclSpace Space Row Col
  --
  | Port Port Row Col
  | DeclType DeclType Row Col
  | DeclDef Name.Name DeclDef Row Col
  --
  | DeclFreshLineStart Row Col
  | DeclFreshLineAfterDocComment Row Col


data DeclDef
  = DeclDefSpace Space Row Col
  | DeclDefEquals Row Col
  | DeclDefType Type Row Col
  | DeclDefArg Pattern Row Col
  | DeclDefBody Expr Row Col
  | DeclDefNameRepeat Row Col
  | DeclDefNameMatch Name.Name Row Col
  --
  | DeclDefIndentType Row Col
  | DeclDefIndentEquals Row Col
  | DeclDefIndentBody Row Col
  --
  | DeclDefFreshLineAfterType Row Col


data Port
  = PortSpace Space Row Col
  | PortName Row Col
  | PortColon Row Col
  | PortType Type Row Col
  | PortIndentName Row Col
  | PortIndentColon Row Col
  | PortIndentType Row Col



-- TYPE DECLARATIONS


data DeclType
  = DT_Space Space Row Col
  | DT_Name Row Col
  | DT_Alias TypeAlias Row Col
  | DT_Union CustomType Row Col
  --
  | DT_IndentName Row Col


data TypeAlias
  = AliasSpace Space Row Col
  | AliasName Row Col
  | AliasEquals Row Col
  | AliasBody Type Row Col
  --
  | AliasIndentEquals Row Col
  | AliasIndentBody Row Col


data CustomType
  = CT_Space Space Row Col
  | CT_Name Row Col
  | CT_Equals Row Col
  | CT_Bar Row Col
  | CT_Variant Row Col
  | CT_VariantArg Type Row Col
  --
  | CT_IndentEquals Row Col
  | CT_IndentBar Row Col
  | CT_IndentAfterBar Row Col
  | CT_IndentAfterEquals Row Col



-- EXPRESSIONS


data Expr
  = Let Let Row Col
  | Case Case Row Col
  | If If Row Col
  | List List Row Col
  | Record Record Row Col
  | Tuple Tuple Row Col
  | Func Func Row Col
  --
  | Dot Row Col
  | Access Row Col
  | OperatorRight Name.Name Row Col
  | OperatorReserved BadOperator Row Col
  --
  | Start Row Col
  | Char Char Row Col
  | String String Row Col
  | Number Number Row Col
  | Space Space Row Col
  | EndlessShader Row Col
  | ShaderProblem [Char.Char] Row Col
  | IndentOperatorRight Name.Name Row Col
  | IndentMoreExpr Row Col


data Record
  = RecordOpen Row Col
  | RecordEnd Row Col
  | RecordField Row Col
  | RecordEquals Row Col
  | RecordExpr Expr Row Col
  | RecordSpace Space Row Col
  --
  | RecordIndentOpen Row Col
  | RecordIndentEnd Row Col
  | RecordIndentField Row Col
  | RecordIndentEquals Row Col
  | RecordIndentExpr Row Col


data Tuple
  = TupleExpr Expr Row Col
  | TupleSpace Space Row Col
  | TupleEnd Row Col
  | TupleOperatorClose Row Col
  | TupleOperatorReserved BadOperator Row Col
  --
  | TupleIndentExpr1 Row Col
  | TupleIndentExprN Row Col
  | TupleIndentEnd Row Col


data List
  = ListSpace Space Row Col
  | ListOpen Row Col
  | ListExpr Expr Row Col
  | ListEnd Row Col
  --
  | ListIndentOpen Row Col
  | ListIndentEnd Row Col
  | ListIndentExpr Row Col


data Func
  = FuncSpace Space Row Col
  | FuncArg Pattern Row Col
  | FuncBody Expr Row Col
  | FuncArrow Row Col
  --
  | FuncIndentArg Row Col
  | FuncIndentArrow Row Col
  | FuncIndentBody Row Col


data Case
  = CaseSpace Space Row Col
  | CaseOf Row Col
  | CasePattern Pattern Row Col
  | CaseArrow Row Col
  | CaseExpr Expr Row Col
  | CaseBranch Expr Row Col
  --
  | CaseIndentOf Row Col
  | CaseIndentExpr Row Col
  | CaseIndentPattern Row Col
  | CaseIndentArrow Row Col
  | CaseIndentBranch Row Col
  | CasePatternAlignment Word16 Row Col


data If
  = IfSpace Space Row Col
  | IfThen Row Col
  | IfElse Row Col
  | IfElseBranchStart Row Col
  --
  | IfCondition Expr Row Col
  | IfThenBranch Expr Row Col
  | IfElseBranch Expr Row Col
  --
  | IfIndentCondition Row Col
  | IfIndentThen Row Col
  | IfIndentThenBranch Row Col
  | IfIndentElseBranch Row Col
  | IfIndentElse Row Col


data Let
  = LetSpace Space Row Col
  | LetIn Row Col
  | LetDefAlignment Word16 Row Col
  | LetDefName Row Col
  | LetDef Name.Name Def Row Col
  | LetDestruct Destruct Row Col
  | LetBody Expr Row Col
  | LetIndentDef Row Col
  | LetIndentIn Row Col
  | LetIndentBody Row Col


data Def
  = DefSpace Space Row Col
  | DefType Type Row Col
  | DefNameRepeat Name.Name Row Col
  | DefNameMatch Name.Name Name.Name Row Col
  | DefArg Pattern Row Col
  | DefEquals Row Col
  | DefBody Expr Row Col
  | DefIndentEquals Row Col
  | DefIndentType Row Col
  | DefIndentBody Row Col
  | DefAlignment Word16 Row Col


data Destruct
  = DestructSpace Space Row Col
  | DestructPattern Pattern Row Col
  | DestructEquals Row Col
  | DestructBody Expr Row Col
  | DestructIndentEquals Row Col
  | DestructIndentBody Row Col



-- PATTERNS


data Pattern
  = PRecord PRecord Row Col
  | PTuple PTuple Row Col
  | PList PList Row Col
  --
  | PStart Row Col
  | PChar Char Row Col
  | PString String Row Col
  | PNumber Number Row Col
  | PFloat Int Row Col
  | PAlias Row Col
  | PWildcardNotVar Int Row Col
  | PSpace Space Row Col
  --
  | PIndentStart Row Col
  | PIndentAlias Row Col


data PRecord
  = PRecordOpen Row Col
  | PRecordEnd Row Col
  | PRecordField Row Col
  | PRecordSpace Space Row Col
  --
  | PRecordIndentOpen Row Col
  | PRecordIndentEnd Row Col
  | PRecordIndentField Row Col


data PTuple
  = PTupleOpen Row Col
  | PTupleEnd Row Col
  | PTupleExpr Pattern Row Col
  | PTupleSpace Space Row Col
  --
  | PTupleIndentOpen Row Col
  | PTupleIndentEnd Row Col
  | PTupleIndentExpr Row Col


data PList
  = PListOpen Row Col
  | PListEnd Row Col
  | PListExpr Pattern Row Col
  | PListSpace Space Row Col
  --
  | PListIndentOpen Row Col
  | PListIndentEnd Row Col
  | PListIndentExpr Row Col



-- TYPES


data Type
  = TRecord TRecord Row Col
  | TTuple TTuple Row Col
  | TVariant Row Col
  | TArrow Row Col
  --
  | TStart Row Col
  | TSpace Space Row Col
  --
  | TIndentStart Row Col


data TRecord
  = TRecordOpen Row Col
  | TRecordEnd Row Col
  --
  | TRecordField Row Col
  | TRecordColon Row Col
  | TRecordType Type Row Col
  --
  | TRecordSpace Space Row Col
  --
  | TRecordIndentOpen Row Col
  | TRecordIndentField Row Col
  | TRecordIndentColon Row Col
  | TRecordIndentType Row Col
  | TRecordIndentEnd Row Col


data TTuple
  = TTupleOpen Row Col
  | TTupleEnd Row Col
  | TTupleType Type Row Col
  | TTupleSpace Space Row Col
  --
  | TTupleIndentOpen Row Col
  | TTupleIndentType Row Col
  | TTupleIndentEnd Row Col



-- LITERALS


data Char
  = CharEndless
  | CharEscape Escape
  | CharNotString Word16


data String
  = StringEndless_Single
  | StringEndless_Multi
  | StringEscape Escape


data Escape
  = EscapeUnknown
  | BadUnicodeFormat Word16
  | BadUnicodeCode Word16
  | BadUnicodeLength Word16 Int Int


data Number
  = NumberEnd
  | NumberDot Int
  | NumberHexDigit
  | NumberNoLeadingZero



-- MISC


data Space
  = HasTab
  | EndlessMultiComment



-- TO REPORT


toReport :: Code.Source -> Error -> Report.Report
toReport source err =
  case err of
    ModuleNameUnspecified name ->
      let
        region = toRegion 1 1
      in
      Report.Report "MODULE NAME MISSING" region [] $
        D.stack
          [ D.reflow $
              "I need the module name to be declared at the top of this file, like this:"
          , D.indent 4 $ D.green $ D.fromChars $
              "module " ++ ModuleName.toChars name ++ " exposing (..)"
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
                  "I need it to match the file path, so I was expecting it to see `"
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
                  ["Switch","this","to","say",D.green "port module","instead,"
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
              ["Switch","this","to",D.green "module"
              ,"and","you","should","be","all","set!"
              ]
          )

    ParseError modul ->
      toParseErrorReport source modul


toParseErrorReport :: Code.Source -> Module -> Report.Report
toParseErrorReport source modul =
  case modul of
    ModuleSpace space row col ->
      toSpaceReport source space row col

    ModuleEndOfFile row col ->
      let
        region = toRegion row col
      in
      Report.Report "EXPECTING END OF FILE" region [] $
        Code.toSnippet source region Nothing
          (
            D.reflow $
              "I was not expecting to see anything more here:"
          ,
            D.reflow $
              "I think I have read all the declarations in the file and that there is nothing\
              \ left to see, so whatever I am running into is confusing me a lot!"
          )

    ModuleProblem row col ->
      let
        region = toRegion row col
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

    ModuleName row col ->
      let
        region = toRegion row col
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

    ModuleExposing exposing row col ->
      error "TODO ModuleExposing" exposing row col

    PortModuleProblem row col ->
      let
        region = toRegion row col
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

    PortModuleName row col ->
      let
        region = toRegion row col
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

    PortModuleExposing exposing row col ->
      error "TODO PortModuleExposing" exposing row col

    Effect row col ->
      let
        region = toRegion row col
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

    FreshLine row col ->
      let
        region = toRegion row col

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
      case Code.whatIsNext source row col of
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
                  , D.vcat
                      [ D.fillSep [D.cyan "import","Html"]
                      , ""
                      , D.fillSep [D.green "main","="]
                      , D.indent 2 $ D.fillSep [D.cyan "Html" <> ".text",D.dullyellow "\"Hello!\""]
                      ]
                  , D.reflow $
                      "You should be able to copy those lines directly into your file. Check out\
                      \ some of the examples on https://elm-lang.org for more help getting started!"
                  , D.toSimpleNote $
                      "This can also happen when something is indented too much!"
                  ]
              )

    ImportStart row col ->
      toImportReport source row col

    ImportName row col ->
      let
        region = toRegion row col
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
                  , D.fillSep [D.cyan "import","Html.Attributes"]
                  , D.fillSep [D.cyan "import","Json.Decode"]
                  ]
              , D.reflow $
                  "Notice that the module names all start with capital letters. That is required!"
              ]
          )

    ImportAs row col ->
      toImportReport source row col

    ImportAlias row col ->
      let
        region = toRegion row col
      in
      Report.Report "EXECTING IMPORT ALIAS" region [] $
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
              ]
          )

    ImportExposing row col ->
      toImportReport source row col

    ImportExposingList exposing row col ->
      error "TODO ImportExposingList" exposing row col

    ImportEnd row col ->
      toImportReport source row col

    ImportIndentName row col ->
      toImportReport source row col

    ImportIndentAlias row col ->
      toImportReport source row col

    ImportIndentExposingList row col ->
      let
        region = toRegion row col
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

    Infix row col ->
      let
        region = toRegion row col
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

    Declarations decl _ _ ->
      toDeclarationsReport source decl



-- IMPORTS


toImportReport :: Code.Source -> Row -> Col -> Report.Report
toImportReport source row col =
  let
    region = toRegion row col
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
          ]
      )



-- SPACES


toSpaceReport :: Code.Source -> Space -> Row -> Col -> Report.Report
toSpaceReport source space row col =
  case space of
    HasTab ->
      let
        region = toRegion row col
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

    EndlessMultiComment ->
      let
        region = toWiderRegion row col 2
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


toRegion :: Row -> Col -> A.Region
toRegion row col =
  let
    pos = A.Position row col
  in
  A.Region pos pos


toWiderRegion :: Row -> Col -> Word16 -> A.Region
toWiderRegion row col extra =
  A.Region
    (A.Position row col)
    (A.Position row (col + extra))


toDeclarationsReport :: Code.Source -> Decl -> Report.Report
toDeclarationsReport source decl =
  case decl of
    DeclStart row col ->
      error "TODO DeclStart" row col

    DeclSpace space row col ->
      toSpaceReport source space row col

    Port port row col ->
      error "TODO Port" port row col

    DeclType declType row col ->
      error "TODO DeclType" declType row col

    DeclDef name declDef row col ->
      toDeclDefReport source name declDef row col

    DeclFreshLineStart row col ->
      error "TODO DeclFreshLineStart" row col

    DeclFreshLineAfterDocComment row col ->
      error "TODO DeclFreshLineAfterDocComment" row col


toDeclDefReport :: Code.Source -> Name.Name -> DeclDef -> Row -> Col -> Report.Report
toDeclDefReport source name declDef startRow startCol =
  case declDef of
    DeclDefSpace space row col ->
      toSpaceReport source space row col

    DeclDefEquals row col ->
      error "TODO DeclDefEquals" row col

    DeclDefType tipe row col ->
      error "TODO DeclDefType" tipe row col

    DeclDefArg pattern row col ->
      error "TODO DeclDefArg" pattern row col

    DeclDefBody expr row col ->
      toExprReport source (InDef name startRow startCol) expr row col

    DeclDefNameRepeat row col ->
      let
        surroundings = A.Region (A.Position startRow startCol) (A.Position row col)
        region = toRegion row col
      in
      Report.Report "EXPECTING DEFINITION" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I just saw the type annotation for `" ++ Name.toChars name
              ++ "` so I was expecting to see its definition here:"
          ,
            D.reflow $
              "Type annotations always appear directly above the relevant\
              \ definition, without anything else (even doc comments) in between."
          )

    DeclDefNameMatch defName row col ->
      let
        surroundings = A.Region (A.Position startRow startCol) (A.Position row col)
        region = toRegion row col
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

    DeclDefIndentType row col ->
      error "TODO DeclDefIndentType" row col

    DeclDefIndentEquals row col ->
      error "TODO DeclDefIndentEquals" row col

    DeclDefIndentBody row col ->
      error "TODO DeclDefIndentBody" row col

    DeclDefFreshLineAfterType row col ->
      error "TODO DeclDefFreshLineAfterType" row col



-- CONTEXT


data Context
  = InNode Node Row Col Context
  | InDef Name.Name Row Col


data Node
  = NRecord
  | NParens
  | NList
  | NFunc
  | NCond
  | NThen
  | NElse
  deriving (Eq)


getDefName :: Context -> Name.Name
getDefName context =
  case context of
    InDef name _ _ -> name
    InNode _ _ _ c -> getDefName c


isWithin :: Node -> Context -> Bool
isWithin desiredNode context =
  case context of
    InDef _ _ _             -> False
    InNode actualNode _ _ _ -> desiredNode == actualNode



-- EXPR REPORTS


toExprReport :: Code.Source -> Context -> Expr -> Row -> Col -> Report.Report
toExprReport source context expr startRow startCol =
  case expr of
    Let _ _ _ ->
      error "TODO Let"

    Case _ _ _ ->
      error "TODO Case"

    If if_ row col ->
      toIfReport source context if_ row col

    List list row col ->
      toListReport source context list row col

    Record record row col ->
      toRecordReport source context record row col

    Tuple tuple row col ->
      toTupleReport source context tuple row col

    Func func row col ->
      toFuncReport source context func row col

    Dot row col ->
      let region = toRegion row col in
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

    Access row col ->
      let region = toRegion row col in
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

    OperatorRight op row col ->
      let region = toRegion row col in
      Report.Report "MISSING EXPRESSION" region [] $
        Code.toSnippet source region Nothing
          (
            D.reflow $
              "I was expecting to see an expression after this " ++ Name.toChars op ++ " operator:"
          ,
            D.stack
              [ D.fillSep $
                  ["You","can","just","put","anything","for","now,","like"
                  ,D.dullyellow "42","or",D.dullyellow"\"hello\"" <> "."
                  ,"Once","there","is","something","there,","I","can","probably"
                  ,"give","a","more","specific","hint!"
                  ]
              , D.toSimpleNote $
                  "This can also happen if run into reserved words like `let` or `as` unexpectedly.\
                  \ Or if I run into operators in unexpected spots. Point is, there are a\
                  \ couple ways I can get confused and give sort of weird advice!"
              ]
          )

    OperatorReserved operator row col ->
      toOperatorReport source context operator row col

    Start row col ->
      let
        (contextRow, contextCol, aThing) =
          case context of
            InDef name r c       -> (r, c, "the `" ++ Name.toChars name ++ "` definition")
            InNode NRecord r c _ -> (r, c, "a record")
            InNode NParens r c _ -> (r, c, "some parentheses")
            InNode NList   r c _ -> (r, c, "a list")
            InNode NFunc   r c _ -> (r, c, "an anonymous function")
            InNode NCond   r c _ -> (r, c, "an `if` expression")
            InNode NThen   r c _ -> (r, c, "an `if` expression")
            InNode NElse   r c _ -> (r, c, "an `if` expression")
            -- TODO check if this is actually the best way to get error messages here

        surroundings = A.Region (A.Position contextRow contextCol) (A.Position row col)
        region = toRegion row col
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
                  "This can also happen if run into reserved words like `let` or `as` unexpectedly.\
                  \ Or if I run into operators in unexpected spots. Point is, there are a\
                  \ couple ways I can get confused and give sort of weird advice!"
              ]
          )

    Char char row col ->
      toCharReport source char row col

    String string row col ->
      toStringReport source string row col

    Number number row col ->
      toNumberReport source number row col

    Space space row col ->
      toSpaceReport source space row col

    EndlessShader row col ->
      let
        region = toWiderRegion row col 6
      in
      Report.Report "ENDLESS SHADER" region [] $
        Code.toSnippet source region Nothing
          (
            D.reflow "I cannot find the end of this shader:"
          ,
            D.reflow "Add a |] somewhere after this to end the shader."
          )

    ShaderProblem problem row col ->
      let
        region = toRegion row col
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

    IndentOperatorRight op row col ->
      let
        surroundings = A.Region (A.Position startRow startCol) (A.Position row col)
        region = toRegion row col
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

    IndentMoreExpr _ _ ->
      error "TODO IndentMoreExpr"



-- CHAR


toCharReport :: Code.Source -> Char -> Row -> Col -> Report.Report
toCharReport source char row col =
  case char of
    CharEndless ->
      let
        region = toRegion row col
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
      toEscapeReport source escape row col

    CharNotString width ->
      let
        region = toWiderRegion row col width
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


toStringReport :: Code.Source -> String -> Row -> Col -> Report.Report
toStringReport source string row col =
  case string of
    StringEndless_Single ->
      let
        region = toRegion row col
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
              , D.link "Note" "Read" "TODO" "if you want a string that spans multiple lines."
              ]
          )

    StringEndless_Multi ->
      let
        region = toWiderRegion row col 3
      in
      Report.Report "ENDLESS STRING" region [] $
        Code.toSnippet source region Nothing
          (
            D.reflow $
              "I cannot find the end of this multi-line string:"
          ,
            D.stack
              [ D.reflow "Add a \"\"\" somewhere after this to end the string."
              , D.link "Hint" "Read" "TODO" "for more information on strings and multi-line strings."
              ]
          )

    StringEscape escape ->
      toEscapeReport source escape row col



-- ESCAPES


toEscapeReport :: Code.Source -> Escape -> Row -> Col -> Report.Report
toEscapeReport source escape row col =
  case escape of
    EscapeUnknown ->
      let
        region = toWiderRegion row col 2
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
        region = toWiderRegion row col width
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
        region = toWiderRegion row col width
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
        region = toWiderRegion row col width
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


toNumberReport :: Code.Source -> Number -> Row -> Col -> Report.Report
toNumberReport source number row col =
  let
    region = toRegion row col
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


toOperatorReport :: Code.Source -> Context -> BadOperator -> Row -> Col -> Report.Report
toOperatorReport source context operator row col =
  case operator of
    BadDot ->
      let
        region = toRegion row col
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
        region = toRegion row col
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
        region = toWiderRegion row col 2
      in
      Report.Report "UNEXPECTED ARROW" region [] $
        Code.toSnippet source region Nothing $
          if error "TODO inCase" then
            (
              D.reflow $
                "I was not expecting to see an arrow here:"
            ,
              D.stack
                [
                  D.reflow $
                    "I think I am parsing a `case` expression right now, so it makes sense to\
                    \ see arrows around here. This one is confusing me though!"
                ,
                  D.toSimpleHint $
                    "I may be getting confused by the indentation. Every pattern in a `case` must be\
                    \ exactly aligned (with exactly the same amount of indentation) so maybe this arrow\
                    \ appears after the next `case` pattern (correctly!) but the pattern itself is\
                    \ indented too far?"
                ]
            )

          else
            (
              D.reflow $
                "I was not expecting an arrow here:"
            ,
              D.reflow $
                "Arrows should only appear in `case` expressions and anonymous functions. Maybe\
                \ you want > or >= instead?"
            )

    BadEquals ->
      let
        region = toRegion row col
        name = getDefName context
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
                    "I may be getting confused by your indentation. Is this supposed to be part of\
                    \ a definition AFTER the `" ++ Name.toChars name ++ "` definition? If so,\
                    \ the problem may be a bit before the equals sign. I need all definitions to\
                    \ be exactly aligned (with exactly the same indentation) so the problem may be that\
                    \ this new definition is indented a bit too much."
              ]
          )

    BadHasType ->
      let
        region = toRegion row col
        name = getDefName context
      in
      Report.Report "UNEXPECTED SYMBOL" region [] $
        Code.toSnippet source region Nothing $
          (
            D.reflow $
              "I was not expecting to run into the \"has type\" symbol here:"
          ,
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



-- IF


toIfReport :: Code.Source -> Context -> If -> Row -> Col -> Report.Report
toIfReport source context if_ startRow startCol =
  case if_ of
    IfSpace space row col ->
      toSpaceReport source space row col

    IfThen row col ->
      let
        surroundings = A.Region (A.Position startRow startCol) (A.Position row col)
        region = toRegion row col
      in
      Report.Report "UNFINISHED IF" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I was expecting to see more of this `if` expression, but I got stuck here:"
          ,
            D.fillSep $
              ["I","was","expecting","to","see","the",D.green "then","keyword","next."
              ]
          )

    IfElse row col ->
      let
        surroundings = A.Region (A.Position startRow startCol) (A.Position row col)
        region = toRegion row col
      in
      Report.Report "UNFINISHED IF" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I was expecting to see more of this `if` expression, but I got stuck here:"
          ,
            D.fillSep $
              ["I","was","expecting","to","see","the",D.green "else","keyword","next."
              ]
          )

    IfElseBranchStart row col ->
      let
        surroundings = A.Region (A.Position startRow startCol) (A.Position row col)
        region = toRegion row col
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

    IfCondition expr row col ->
      toExprReport source (InNode NCond startRow startCol context) expr row col

    IfThenBranch expr row col ->
      toExprReport source (InNode NThen startRow startCol context) expr row col

    IfElseBranch expr row col ->
      toExprReport source (InNode NElse startRow startCol context) expr row col

    IfIndentCondition row col ->
      let
        surroundings = A.Region (A.Position startRow startCol) (A.Position row col)
        region = toRegion row col
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

    IfIndentThen row col ->
      let
        surroundings = A.Region (A.Position startRow startCol) (A.Position row col)
        region = toRegion row col
      in
      Report.Report "UNFINISHED IF" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I was expecting to see more of this `if` expression, but I got stuck here:"
          ,
            D.stack
              [ D.fillSep $
                  ["I","was","expecting","to","see","the",D.green "then","keyword","next."
                  ]
              , D.toSimpleNote $
                  "I can be confused by indentation. Maybe something is not indented enough?"
              ]
          )

    IfIndentThenBranch row col ->
      let
        surroundings = A.Region (A.Position startRow startCol) (A.Position row col)
        region = toRegion row col
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

    IfIndentElseBranch row col ->
      let
        surroundings = A.Region (A.Position startRow startCol) (A.Position row col)
        region = toRegion row col
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

    IfIndentElse row col ->
      let
        surroundings = A.Region (A.Position startRow startCol) (A.Position row col)
        region = toRegion row col
      in
      Report.Report "UNFINISHED IF" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I was expecting to see an `else` branch after this:"
          ,
            D.stack
              [ D.reflow $
                  "I know what to do when the condition is `True`, but not when it is `False`."
              , D.fillSep
                  ["Add","an",D.green "else","branch","to","handle","that","scenario!"]
              ]
          )



-- RECORD


toRecordReport :: Code.Source -> Context -> Record -> Row -> Col -> Report.Report
toRecordReport source context record startRow startCol =
  case record of
    RecordOpen row col ->
      let
        surroundings = A.Region (A.Position startRow startCol) (A.Position row col)
        region = toRegion row col
      in
      case Code.whatIsNext source row col of
        Code.Keyword keyword ->
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
          Report.Report "UNFINISHED RECORD" region [] $
            Code.toSnippet source surroundings (Just region)
              (
                D.reflow $
                  "I just started parsing a record, but I got stuck here:"
              ,
                D.fillSep
                  ["Records","look","like",D.dullyellow "{ x = 3, y = 4 },"
                  ,"so","I","was","expecting","to","see","a","field","name","next."
                  ]
              )

    RecordEnd row col ->
      let
        surroundings = A.Region (A.Position startRow startCol) (A.Position row col)
        region = toRegion row col
      in
      Report.Report "UNFINISHED RECORD" region [] $
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

    RecordField row col ->
      let
        surroundings = A.Region (A.Position startRow startCol) (A.Position row col)
        region = toRegion row col
      in
      case Code.whatIsNext source row col of
        Code.Keyword keyword ->
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

        _ ->
          Report.Report "UNFINISHED RECORD" region [] $
            Code.toSnippet source surroundings (Just region)
              (
                D.reflow $
                  "I am partway through parsing a record, but I got stuck after that last comma:"
              ,
                addNoteForRecordError $
                  D.reflow $
                    "Trailing commas are not allowed in records, so I was expecting to see another\
                    \ record field defined next. If you are done defining the record, the fix may\
                    \ be to delete that last comma?"
              )

    RecordEquals row col ->
      let
        surroundings = A.Region (A.Position startRow startCol) (A.Position row col)
        region = toRegion row col
      in
      Report.Report "UNFINISHED RECORD" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I am partway through parsing a record, but I got stuck here:"
          ,
            addNoteForRecordError $
              D.fillSep $
                ["I","just","saw","a","field","name,","so","I","was","expecting","to","see"
                ,"an","equals","sign","next.","So","try","putting","an",D.green "=","sign","here?"
                ]
          )

    RecordExpr expr row col ->
      toExprReport source (InNode NRecord startRow startCol context) expr row col

    RecordSpace space row col ->
      toSpaceReport source space row col

    RecordIndentOpen row col ->
      let
        surroundings = A.Region (A.Position startRow startCol) (A.Position row col)
        region = toRegion row col
      in
      Report.Report "UNFINISHED RECORD" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I just saw the opening curly brace of a record, but then I got stuck here:"
          ,
            addNoteForRecordIndentError $
              D.fillSep $
                ["I","am","expecting","a","record","like",D.dullyellow "{ x = 3, y = 4 }","here."
                ,"Try","defining","some","fields","of","your","own?"
                ]
          )

    RecordIndentEnd row col ->
      let
        surroundings = A.Region (A.Position startRow startCol) (A.Position row col)
        region = toRegion row col
      in
      Report.Report "UNFINISHED RECORD" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I was expecting to see a closing curly brace next:"
          ,
            addNoteForRecordIndentError $
              D.fillSep $
                ["Try","putting","an",D.green "}","and","see","if","that","helps?"
                ]
          )

    RecordIndentField row col ->
      let
        surroundings = A.Region (A.Position startRow startCol) (A.Position row col)
        region = toRegion row col
      in
      Report.Report "UNFINISHED RECORD" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I am partway through parsing a record, but I got stuck after that last comma:"
          ,
            addNoteForRecordError $
              D.reflow $
                "Trailing commas are not allowed in records, so the fix may be to\
                \ delete that last comma? Or maybe you were in the middle of defining\
                \ an additional field?"
          )

    RecordIndentEquals row col ->
      let
        surroundings = A.Region (A.Position startRow startCol) (A.Position row col)
        region = toRegion row col
      in
      Report.Report "UNFINISHED RECORD" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I am partway through parsing a record. I just saw a record\
              \ field, so I was expecting to see an equals sign next:"
          ,
            addNoteForRecordIndentError $
              D.fillSep $
                ["Try","putting","an",D.green "=","followed","by","an","expression?"
                ]
          )

    RecordIndentExpr row col ->
      let
        surroundings = A.Region (A.Position startRow startCol) (A.Position row col)
        region = toRegion row col
      in
      Report.Report "UNFINISHED RECORD" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I am partway through parsing a record, and I was expecting to run into an expression next:"
          ,
            addNoteForRecordIndentError $
              D.fillSep $
                ["Try","putting","something,","like"
                ,D.dullyellow "42","or",D.dullyellow"\"hello\"","for","now?"
                ]
          )


addNoteForRecordError :: D.Doc -> D.Doc
addNoteForRecordError normalRecommendation =
  D.stack $
    normalRecommendation
    :
    [ D.toSimpleNote
        "If you are trying to define a record across multiple lines, I recommend using this format:"
    , D.indent 4 $ D.vcat $
        [ "{ name = \"Alice\""
        , ", age = 42"
        , ", height = 1.75"
        , "}"
        ]
    , D.reflow $
        "Notice that nothing comes directly after a newline. Each line starts with some spaces. I\
        \ know this style can be jarring for people coming from C-like syntax, but folks generally\
        \ report that they are used to it (and often prefer it!) after a week or two of using Elm."
    ]


addNoteForRecordIndentError :: D.Doc -> D.Doc
addNoteForRecordIndentError normalRecommendation =
  D.stack $
    normalRecommendation
    :
    [ D.toSimpleNote
        "I may be confused by indentation. For example, if you are trying to define\
        \ a record across multiple lines, I recommend using this format:"
    , D.indent 4 $ D.vcat $
        [ "[ \"Alice\""
        , ", \"Bob\""
        , ", \"Chuck\""
        , "]"
        ]
    , D.reflow $
        "Notice that nothing comes directly after a newline. Each line starts with some spaces. I\
        \ know this style can be jarring for people coming from C-like syntax, but folks generally\
        \ report that they are used to it (and often prefer it!) after a week or two of using Elm."
    ]



-- TUPLE


toTupleReport :: Code.Source -> Context -> Tuple -> Row -> Col -> Report.Report
toTupleReport source context tuple startRow startCol =
  case tuple of
    TupleExpr expr row col ->
      toExprReport source (InNode NParens startRow startCol context) expr row col

    TupleSpace space row col ->
      toSpaceReport source space row col

    TupleEnd row col ->
      let
        surroundings = A.Region (A.Position startRow startCol) (A.Position row col)
        region = toRegion row col
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

    TupleOperatorClose row col ->
      let
        surroundings = A.Region (A.Position startRow startCol) (A.Position row col)
        region = toRegion row col
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

    TupleOperatorReserved operator row col ->
      let
        surroundings = A.Region (A.Position startRow startCol) (A.Position row col)
        region = toRegion row col
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

    TupleIndentExpr1 row col ->
      let
        surroundings = A.Region (A.Position startRow startCol) (A.Position row col)
        region = toRegion row col
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

    TupleIndentExprN row col ->
      let
        surroundings = A.Region (A.Position startRow startCol) (A.Position row col)
        region = toRegion row col
      in
      Report.Report "UNFINISHED TUPLE" region [] $
        Code.toSnippet source surroundings (Just region)
          (
            D.reflow $
              "I think I am in the middle of parsing a tuple. I just saw a comma, so I was expecting to see expression next."
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

    TupleIndentEnd row col ->
      let
        surroundings = A.Region (A.Position startRow startCol) (A.Position row col)
        region = toRegion row col
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


toListReport :: Code.Source -> Context -> List -> Row -> Col -> Report.Report
toListReport source context list startRow startCol =
  case list of
    ListSpace space row col ->
      toSpaceReport source space row col

    ListOpen row col ->
      let
        surroundings = A.Region (A.Position startRow startCol) (A.Position row col)
        region = toRegion row col
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

    ListExpr expr row col ->
      case expr of
        Start r c ->
          let
            surroundings = A.Region (A.Position startRow startCol) (A.Position r c)
            region = toRegion r c
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
                      [ "[ \"Alice\""
                      , ", \"Bob\""
                      , ", \"Chuck\""
                      , "]"
                      ]
                  , D.reflow $
                      "Notice that nothing comes directly after a newline. Each line starts with some spaces.\
                      \ This style can be jarring for people coming from C-like syntax, but folks generally\
                      \ report that they are used to it (and often prefer it!) after a week or two of using Elm."
                  ]
              )

        _ ->
          toExprReport source (InNode NList startRow startCol context) expr row col

    ListEnd row col ->
      let
        surroundings = A.Region (A.Position startRow startCol) (A.Position row col)
        region = toRegion row col
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

    ListIndentOpen row col ->
      let
        surroundings = A.Region (A.Position startRow startCol) (A.Position row col)
        region = toRegion row col
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
                  [ "[ \"Alice\""
                  , ", \"Bob\""
                  , ", \"Chuck\""
                  , "]"
                  ]
              , D.reflow $
                  "Notice that nothing comes directly after a newline. Each line starts with some spaces.\
                  \ This style can be jarring for people coming from C-like syntax, but folks generally\
                  \ report that they are used to it (and often prefer it!) after a week or two of using Elm."
              ]
          )

    ListIndentEnd row col ->
      let
        surroundings = A.Region (A.Position startRow startCol) (A.Position row col)
        region = toRegion row col
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
                  [ "[ \"Alice\""
                  , ", \"Bob\""
                  , ", \"Chuck\""
                  , "]"
                  ]
              , D.reflow $
                  "Notice that nothing comes directly after a newline. Each line starts with some spaces.\
                  \ This style can be jarring for people coming from C-like syntax, but folks generally\
                  \ report that they are used to it (and often prefer it!) after a week or two of using Elm."
              ]
          )

    ListIndentExpr row col ->
      let
        surroundings = A.Region (A.Position startRow startCol) (A.Position row col)
        region = toRegion row col
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
                  [ "[ \"Alice\""
                  , ", \"Bob\""
                  , ", \"Chuck\""
                  , "]"
                  ]
              , D.reflow $
                  "Notice that nothing comes directly after a newline. Each line starts with some spaces.\
                  \ This style can be jarring for people coming from C-like syntax, but folks generally\
                  \ report that they are used to it (and often prefer it!) after a week or two of using Elm."
              ]
          )


toFuncReport :: Code.Source -> Context -> Func -> Row -> Col -> Report.Report
toFuncReport source context func startRow startCol =
  case func of
    FuncSpace space row col ->
      toSpaceReport source space row col

    FuncArg pattern row col ->
      error "TODO FuncArg" pattern row col

    FuncBody expr row col ->
      toExprReport source (InNode NFunc startRow startCol context) expr row col

    FuncArrow row col ->
      let
        surroundings = A.Region (A.Position startRow startCol) (A.Position row col)
        region = toRegion row col
      in
      case Code.whatIsNext source row col of
        Code.Keyword keyword ->
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

    FuncIndentArg row col ->
      let
        surroundings = A.Region (A.Position startRow startCol) (A.Position row col)
        region = toRegion row col
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

    FuncIndentArrow row col ->
      let
        surroundings = A.Region (A.Position startRow startCol) (A.Position row col)
        region = toRegion row col
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

    FuncIndentBody row col ->
      let
        surroundings = A.Region (A.Position startRow startCol) (A.Position row col)
        region = toRegion row col
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




{-
  case err of
    CommentOnNothing region ->
      Report.Report "STRAY COMMENT" region [] $
        Code.toSnippet source region Nothing
          (
            "This documentation comment is not followed by anything."
          ,
            D.reflow $
              "All documentation comments need to be right above the declaration they\
              \ describe. Maybe some code got deleted or commented out by accident? Or\
              \ maybe this comment is here by accident?"
          )

    ParseError row col context expectation ->
      let
        pos = A.Position row col
        region = A.Region pos pos
      in
      case expectation of

        DocComment ->
          Report.Report "EXPECTING DOC COMMENT" region [] $
            Code.toSnippet source region Nothing
              (
                D.reflow $
                  "I was expecting a doc comment here:"
              ,
                D.reflowLink
                  "Check out" "TODO" "for examples of doc comments."
              )

        -- Parse.Number

        Wildcard ->
          Report.Report "EXPECTING A WILDCARD" region [] $
            Code.toSnippet source region Nothing
              (
                D.reflow $
                  "I was expecting a _ here:"
              ,
                D.reflow $
                  "An underscore indicates that “I do not care about this value” and is\
                  \ called a wildcard pattern because it will match with any type of values."
              )

        WildcardNotVar startCol endCol ->
          let
            badRegion =
              A.Region (A.Position row startCol) (A.Position row endCol)
          in
          Report.Report "INVALID PATTERN" badRegion [] $
            Code.toSnippet source badRegion Nothing
              (
                D.reflow $
                  "I was expecting to see an underscore without any additional characters:"
              ,
                D.reflow $
                  "There are two options. Either (1) remove the extra characters and ignore the\
                  \ value or (2) remove the starting underscore to give the value a normal name."
              )

        -- Parse.Pattern

        FloatInPattern ->
          Report.Report "INVALID PATTERN" region [] $
            Code.toSnippet source region Nothing
              (
                D.reflow $
                  "I cannot pattern match with floating point numbers:"
              ,
                D.reflow $
                  "Equality on floats can be unreliable, so you usually want to check that they\
                  \ are nearby with some sort of (abs (actual - expected) < 0.001) check."
              )
-}
