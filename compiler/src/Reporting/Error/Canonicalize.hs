{-# LANGUAGE OverloadedStrings #-}
module Reporting.Error.Canonicalize
  ( Error(..)
  , Problem(..)
  , InvalidPayload(..)
  , PortProblem(..)
  , DuplicatePatternContext(..)
  , PossibleNames(..)
  , toReport
  )
  where


import Prelude hiding (cycle)
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Graph

import qualified AST.Source as Src
import qualified AST.Prim.Module as Module
import qualified AST.Prim.Name as N
import qualified AST.Prim.Operator as Op
import qualified AST.Prim.TypeName as T
import qualified AST.Prim.TypeVar as T
import qualified Data.Index as Index
import qualified Elm.ModuleName as ModuleName
import qualified Reporting.Ambiguous as Amb
import qualified Reporting.Annotation as A
import qualified Reporting.Doc as D
import Reporting.Doc (Doc, (<+>))
import qualified Reporting.Render.Code as Code
import qualified Reporting.Render.Type as RT
import qualified Reporting.Report as Report
import qualified Reporting.Suggest as Suggest



-- CANONICALIZATION ERRORS


data Error
  = ModuleNameUnspecified Module.Name A.Region
  | ModuleNameMismatch Module.Name (A.Located Module.Name)
  | UnexpectedPort A.Region
  | NoPorts A.Region
  | NoPortsInPackage (A.Located N.Name)
  | NoPortModulesInPackage A.Region
  | NoEffectsOutsideKernel A.Region
  | EffectNotFound A.Region T.Name
  | EffectFunctionNotFound A.Region N.Name
  --
  | ImportCtorByName A.Region N.Name T.Name
  | ImportNotFound A.Region Module.Name [ModuleName.Canonical]
  | ImportOpenAlias A.Region T.Name
  | ImportExposedValueNotFound A.Region ModuleName.Canonical N.Name [N.Name]
  | ImportExposedTypeNotFound A.Region ModuleName.Canonical T.Name [T.Name]
  | ImportExposedBinopNotFound A.Region ModuleName.Canonical Op.Name [Op.Name]
  --
  | PortPayloadInvalid A.Region N.Name InvalidPayload
  | PortTypeInvalid A.Region N.Name PortProblem
  --
  | NoDecls A.Region
  --
  | Problem Problem


data Problem
  = AnnotationTooShort A.Region N.Name Index.ZeroBased Int
  | AmbiguousVar  A.Region (Maybe Module.Prefix) N.Name (Amb.Ambiguous Module.Name)
  | AmbiguousType A.Region (Maybe Module.Prefix) T.Name (Amb.Ambiguous Module.Name)
  | AmbiguousCtor A.Region (Maybe Module.Prefix) N.Name (Amb.Ambiguous Module.Name)
  | BadArity_Type A.Region T.Name Int Int
  | BadArity_Pattern A.Region N.Name Int Int
  | Binop A.Region Op.Name Op.Name
  | DuplicateBinop Op.Name A.Region A.Region
  | DuplicateDecl N.Name A.Region A.Region
  | DuplicateType T.Name A.Region A.Region
  | DuplicateCtor N.Name A.Region A.Region
  | DuplicateField N.Name A.Region A.Region
  | DuplicateAliasArg T.Name T.Var A.Region A.Region
  | DuplicateUnionArg T.Name T.Var A.Region A.Region
  | DuplicatePattern DuplicatePatternContext N.Name A.Region A.Region
  | NotFoundVar A.Region (Maybe Module.Prefix) N.Name (PossibleNames N.Name)
  | NotFoundType A.Region (Maybe Module.Prefix) T.Name (PossibleNames T.Name)
  | NotFoundCtor A.Region (Maybe Module.Prefix) N.Name (PossibleNames N.Name)
  | NotFoundBinop A.Region Op.Name (Set.Set Op.Name)
  | PatternHasRecordCtor A.Region N.Name
  | RecursiveAlias (A.Located T.Name) [T.Var] Src.Type (Graph.MinimalCycle T.Name)
  | RecursiveDecl (A.Located N.Name) (Graph.MinimalCycle N.Name)
  | BadLetRec_Value (A.Located N.Name) (Graph.MinimalCycle N.Name)
  | Shadowing N.Name A.Region A.Region
  | TupleLargerThanThree A.Region
  | TypeVarsUnboundInUnion  A.Region T.Name [T.Var] (A.Located T.Var) [A.Located T.Var]
  | TypeVarsMessedUpInAlias A.Region T.Name [T.Var] [A.Located T.Var] [A.Located T.Var]


data DuplicatePatternContext
  = DPLambdaArgs
  | DPFuncArgs N.Name
  | DPCaseBranch
  | DPLetBinding
  | DPDestruct


data InvalidPayload
  = ExtendedRecord
  | Function
  | TypeVariable T.Var
  | UnsupportedType T.Name


data PortProblem
  = CmdNoArg
  | CmdExtraArgs Int
  | CmdBadMsg
  | SubBad
  | NotCmdOrSub


data PossibleNames name =
  PossibleNames
    { _locals :: Set.Set name
    , _quals :: Map.Map Module.Prefix (Set.Set name)
    }



-- TO REPORT


toReport :: Code.Source -> Error -> Report.Report
toReport source err =
  case err of
    ModuleNameUnspecified name region ->
      Report.Report "MODULE NAME MISSING" region [] $
        D.stack
          [ D.reflow $
              "I need the module name to be declared at the top of this file, like this:"
          , D.indent 4 $ D.fillSep $
              [ D.cyan "module", D.fromModule name, D.cyan "exposing", "(..)" ]
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
      Report.Report "MODULE NAME MISMATCH" region [Module.toChars expectedName] $
        Code.toSnippet source region Nothing
          (
            "It looks like this module name is out of sync:"
          ,
            D.stack
              [ D.reflow $
                  "I need it to match the file path, so I was expecting to see `"
                  ++ Module.toChars expectedName
                  ++ "` here. Make the following change, and you should be all set!"
              , D.indent 4 $
                  D.dullyellow (D.fromModule actualName) <> " -> " <> D.green (D.fromModule expectedName)
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

    EffectNotFound region name ->
      Report.Report "EFFECT PROBLEM" region [] $
        Code.toSnippet source region Nothing
          (
            D.reflow $
              "You have declared that `" ++ T.nameToChars name ++ "` is an effect type:"
          ,
            D.reflow $
              "But I cannot find a custom type named `" ++ T.nameToChars name ++ "` in this file!"
          )

    EffectFunctionNotFound region name ->
      Report.Report "EFFECT PROBLEM" region [] $
        Code.toSnippet source region Nothing
          (
            D.reflow $
              "This kind of effect module must define a `" ++ N.toChars name ++ "` function."
          ,
            D.reflow $
              "But I cannot find `" ++ N.toChars name ++ "` in this file!"
          )

    ImportCtorByName region ctor tipe ->
      Report.Report "BAD IMPORT" region [] $
        Code.toSnippet source region Nothing
          (
            D.reflow $
              "You are trying to import the `" <> N.toChars ctor
              <> "` variant by name:"
          ,
            D.fillSep
              ["Try","importing",D.green (D.fromType tipe <> "(..)"),"instead."
              ,"The","dots","mean","“expose","the",D.fromType tipe,"type","and"
              ,"all","its","variants","so","it","gives","you","access","to"
              , D.fromName ctor <> "."
              ]
          )

    ImportNotFound region name _ ->
      --
      -- NOTE: this should always be detected by `builder`
      -- So this error should never actually get printed out.
      --
      Report.Report "UNKNOWN IMPORT" region [] $
        Code.toSnippet source region Nothing
          (
            D.reflow $
              "I could not find a `" <> Module.toChars name <> "` module to import!"
          ,
            mempty
          )

    ImportOpenAlias region name ->
      Report.Report "BAD IMPORT" region [] $
        Code.toSnippet source region Nothing
          (
            D.reflow $
              "The `" <> T.nameToChars name <> "` type alias cannot be followed by (..) like this:"
          ,
            D.reflow $
              "Remove the (..) and it should work."
          )

    ImportExposedValueNotFound r h n xs -> importNotFound source r h n xs N.toChars
    ImportExposedTypeNotFound  r h n xs -> importNotFound source r h n xs T.nameToChars
    ImportExposedBinopNotFound r h n xs -> importNotFound source r h n xs Op.toChars

    PortPayloadInvalid region portName invalidPayload ->
      let
        formatDetails (aBadKindOfThing, elaboration) =
          Report.Report "PORT ERROR" region [] $
            Code.toSnippet source region Nothing
              (
                D.reflow $
                  "The `" <> N.toChars portName <> "` port is trying to transmit " <> aBadKindOfThing <> ":"
              ,
                D.stack
                  [ elaboration
                  , D.link "Hint"
                      "Ports are not a traditional FFI, so if you have tons of annoying ports, definitely read"
                      "ports"
                      "to learn how they are meant to work. They require a different mindset!"
                  ]
              )
      in
      formatDetails $
        case invalidPayload of
          ExtendedRecord ->
            (
              "an extended record"
            ,
              D.reflow $
                "But the exact shape of the record must be known at compile time. No type variables!"
            )

          Function ->
            (
              "a function"
            ,
              D.reflow $
                "But functions cannot be sent in and out ports. If we allowed functions in from JS\
                \ they may perform some side-effects. If we let functions out, they could produce\
                \ incorrect results because Elm optimizations assume there are no side-effects."
            )


          TypeVariable name ->
            (
              "an unspecified type"
            ,
              D.reflow $
                "But type variables like `" <> T.varToChars name <> "` cannot flow through ports.\
                \ I need to know exactly what type of data I am getting, so I can guarantee that\
                \ unexpected data cannot sneak in and crash the Elm program."
            )

          UnsupportedType name ->
            (
              "a `" <> T.nameToChars name <> "` value"
            ,
              D.stack
                [ D.reflow $ "I cannot handle that. The types that CAN flow in and out of Elm include:"
                , D.indent 4 $
                    D.reflow $
                      "Ints, Floats, Bools, Strings, Maybes, Lists, Arrays,\
                      \ tuples, records, and JSON values."
                , D.reflow $
                    "Since JSON values can flow through, you can use JSON encoders and decoders\
                    \ to allow other types through as well. More advanced users often just do\
                    \ everything with encoders and decoders for more control and better errors."
                ]
            )

    PortTypeInvalid region name portProblem ->
      let
        formatDetails (before, after) =
          Report.Report "BAD PORT" region [] $
            Code.toSnippet source region Nothing $
              (
                D.reflow before
              ,
                D.stack
                  [ after
                  , D.link "Hint" "Read" "ports"
                      "for more advice. For example, do not end up with one port per JS function!"
                  ]
              )
      in
      formatDetails $
        case portProblem of
          CmdNoArg ->
            (
              "The `" <> N.toChars name <> "` port cannot be just a command."
            ,
              D.reflow $
                "It can be (() -> Cmd msg) if you just need to trigger a JavaScript\
                \ function, but there is often a better way to set things up."
            )

          CmdExtraArgs n ->
            (
              "The `" <> N.toChars name <> "` port can only send ONE value out to JavaScript."
            ,
              let
                theseItemsInSomething
                  | n == 2 = "both of these items into a tuple or record"
                  | n == 3 = "these " ++ show n ++ " items into a tuple or record"
                  | True   = "these " ++ show n ++ " items into a record"
              in
              D.reflow $
                "You can put " ++ theseItemsInSomething ++ " to send them out though."
            )

          CmdBadMsg ->
            (
              "The `" <> N.toChars name <> "` port cannot send any messages to the `update` function."
            ,
              D.reflow $
                "It must produce a (Cmd msg) type. Notice the lower case `msg` type\
                \ variable. The command will trigger some JS code, but it will not send\
                \ anything particular back to Elm."
            )

          SubBad ->
            ( "There is something off about this `" <> N.toChars name <> "` port declaration."
            ,
              D.stack
                [ D.reflow $
                    "To receive messages from JavaScript, you need to define a port like this:"
                , D.indent 4 $ D.dullyellow $ D.fromChars $
                    "port " <> N.toChars name <> " : (Int -> msg) -> Sub msg"
                , D.reflow $
                    "Now every time JS sends an `Int` to this port, it is converted to a `msg`.\
                    \ And if you subscribe, those `msg` values will be piped into your `update`\
                    \ function. The only thing you can customize here is the `Int` type."
                ]
            )

          NotCmdOrSub ->
            (
              "I am confused about the `" <> N.toChars name <> "` port declaration."
            ,
              D.reflow $
                "Ports need to produce a command (Cmd) or a subscription (Sub) but\
                \ this is neither. I do not know how to handle this."
            )

    NoDecls region ->
      Report.Report "NO DECLARATIONS" region [] $
        Code.toSnippet source region Nothing
          (
            D.reflow $
              "This module is empty!"
          ,
            D.stack $
              [ D.reflow $
                  "I was expecting to see some type declaration, functions, etc. I recommend\
                  \ starting an Elm file with definitions like this:"
              , D.indent 4 $ D.vcat $
                  [ D.fillSep [D.cyan "import","Html"]
                  , ""
                  , "main ="
                  , "  Html.text " <> D.dullyellow "\"Hello!\""
                  ]
              , D.reflow $
                  "You should be able to copy those lines directly into your file. Check out the\
                  \ examples at <https://elm-lang.org/examples> for more help getting started!"
              ]
          )

    Problem p ->
      toProblemReport source p


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


toProblemReport :: Code.Source -> Problem -> Report.Report
toProblemReport source problem =
  case problem of
    AnnotationTooShort region name index leftovers ->
      let
        numTypeArgs = Index.toMachine index
        numDefArgs = numTypeArgs + leftovers
      in
      Report.Report "BAD TYPE ANNOTATION" region [] $
        Code.toSnippet source region Nothing
          (
            D.reflow $
              "The type annotation for `" <> N.toChars name <> "` says it can accept "
              <> D.args numTypeArgs <> ", but the definition says it has "
              <> D.args numDefArgs <> ":"
          ,
            D.reflow $
              "Is the type annotation missing something? Should some argument"
              <> (if leftovers == 1 then "" else "s")
              <> " be deleted? Maybe some parentheses are missing?"
          )

    AmbiguousVar  r p n hs -> ambiguousName source r p n hs "variable" N.toChars
    AmbiguousType r p n hs -> ambiguousName source r p n hs "type" T.nameToChars
    AmbiguousCtor r p n hs -> ambiguousName source r p n hs "constructor" N.toChars

    BadArity_Type    r n e a -> badArity source r n e a T.nameToChars "type"
    BadArity_Pattern r n e a -> badArity source r n e a N.toChars "variant"

    Binop region op1 op2 ->
      Report.Report "INFIX PROBLEM" region [] $
        Code.toSnippet source region Nothing
          (
            D.reflow $
              "You cannot mix (" <> Op.toChars op1 <> ") and (" <> Op.toChars op2 <> ") without parentheses."
          ,
            D.reflow
              "I do not know how to group these expressions. Add parentheses for me!"
          )

    DuplicateBinop      n r1 r2 -> nameClash source r1 r2 $ "This file defines multiple (" <> Op.toChars n <> ") operators."
    DuplicateDecl       n r1 r2 -> nameClash source r1 r2 $ "This file has multiple `" <> N.toChars n <> "` declarations."
    DuplicateType       n r1 r2 -> nameClash source r1 r2 $ "This file defines multiple `" <> T.nameToChars n <> "` types."
    DuplicateCtor       n r1 r2 -> nameClash source r1 r2 $ "This file defines multiple `" <> N.toChars n <> "` type constructors."
    DuplicateField      n r1 r2 -> nameClash source r1 r2 $ "This record has multiple `" <> N.toChars n <> "` fields."
    DuplicateAliasArg t n r1 r2 -> nameClash source r1 r2 $ "The `" <> T.nameToChars t <> "` type alias has multiple `" <> T.varToChars n <> "` type variables."
    DuplicateUnionArg t n r1 r2 -> nameClash source r1 r2 $ "The `" <> T.nameToChars t <> "` type has multiple `" <> T.varToChars n <> "` type variables."

    DuplicatePattern context n r1 r2 ->
      nameClash source r1 r2 $
        case context of
          DPLambdaArgs -> "This anonymous function has multiple `" <> N.toChars n <> "` arguments."
          DPFuncArgs f -> "The `" <> N.toChars f <> "` function has multiple `" <> N.toChars n <> "` arguments."
          DPCaseBranch -> "This `case` pattern has multiple `" <> N.toChars n <> "` variables."
          DPLetBinding -> "This `let` expression defines `" <> N.toChars n <> "` more than once!"
          DPDestruct   -> "This pattern contains multiple `" <> N.toChars n <> "` variables."

    NotFoundVar  r p n ns -> notFound source r p n "variable"    ns N.toChars
    NotFoundType r p n ns -> notFound source r p n "type"        ns T.nameToChars
    NotFoundCtor r p n ns -> notFound source r p n "constructor" ns N.toChars

    NotFoundBinop region op locals ->
      if op == "===" then
        Report.Report "UNKNOWN OPERATOR" region ["=="] $
          Code.toSnippet source region Nothing
            (
              "Elm does not have a (===) operator like JavaScript."
            ,
              "Switch to (==) instead."
            )

      else if op == "!=" || op == "!==" then
        Report.Report "UNKNOWN OPERATOR" region ["/="] $
          Code.toSnippet source region Nothing
            (
              D.reflow $
                "Elm uses a different name for the “not equal” operator:"
            ,
              D.stack
                [ D.reflow "Switch to (/=) instead."
                , D.toSimpleNote $
                    "Our (/=) operator is supposed to look like a real “not equal” sign (≠). I hope that history will remember ("
                    ++ Op.toChars op ++ ") as a weird and temporary choice."
                ]
            )

      else if op == "**" then
        Report.Report "UNKNOWN OPERATOR" region ["^","*"] $
          Code.toSnippet source region Nothing
            (
              D.reflow $
                "I do not recognize the (**) operator:"
            ,
              D.reflow $
                "Switch to (^) for exponentiation. Or switch to (*) for multiplication."
            )

      else if op == "%" then
        Report.Report "UNKNOWN OPERATOR" region [] $
          Code.toSnippet source region Nothing
            (
              D.reflow $
                "Elm does not use (%) as the remainder operator:"
            ,
              D.stack
                [ D.reflow $
                    "If you want the behavior of (%) like in JavaScript, switch to:\
                    \ <https://package.elm-lang.org/packages/elm/core/latest/Basics#remainderBy>"
                , D.reflow $
                    "If you want modular arithmetic like in math, switch to:\
                    \ <https://package.elm-lang.org/packages/elm/core/latest/Basics#modBy>"
                , D.reflow $
                    "The difference is how things work when negative numbers are involved."
                ]
            )

      else
        let
          suggestions =
            map Op.toChars $ take 2 $
              Suggest.sort (Op.toChars op) Op.toChars (Set.toList locals)

          format altOp =
            D.green $ "(" <> altOp <> ")"
        in
        Report.Report "UNKNOWN OPERATOR" region suggestions $
          Code.toSnippet source region Nothing
            (
              D.reflow $
                "I do not recognize the (" ++ Op.toChars op ++ ") operator."
            ,
              D.fillSep $
                ["Is","there","an","`import`","and","`exposing`","entry","for","it?"]
                ++
                  case map D.fromChars suggestions of
                    [] ->
                      []

                    alts ->
                      ["Maybe","you","want"] ++ D.commaSep "or" format alts ++ ["instead?"]
            )

    PatternHasRecordCtor region name ->
      Report.Report "BAD PATTERN" region [] $
        Code.toSnippet source region Nothing
          (
            D.reflow $
              "You can construct records by using `" <> N.toChars name
              <> "` as a function, but it is not available in pattern matching like this:"
          ,
            D.reflow $
              "I recommend matching the record as a variable and unpacking it later."
          )

    RecursiveAlias name args tipe others ->
      aliasRecursionReport source name args tipe others

    RecursiveDecl (A.At region name) cycle ->
      let
        makeTheory question details =
          D.fillSep $ map (D.dullyellow . D.fromChars) (words question) ++ map D.fromChars (words details)
      in
      Report.Report "CYCLIC DEFINITION" region [] $
        Code.toSnippet source region Nothing $
          if Graph.isSelfRecursive cycle
          then
              (
                D.reflow $
                  "The `" <> N.toChars name <> "` value is defined directly in terms of itself, causing an infinite loop."
              ,
                D.stack
                  [ makeTheory "Are you trying to mutate a variable?" $
                      "Elm does not have mutation, so when I see " ++ N.toChars name
                      ++ " defined in terms of " ++ N.toChars name
                      ++ ", I treat it as a recursive definition. Try giving the new value a new name!"
                  , makeTheory "Maybe you DO want a recursive value?" $
                      "To define " ++ N.toChars name ++ " we need to know what " ++ N.toChars name
                      ++ " is, so let’s expand it. Wait, but now we need to know what " ++ N.toChars name
                      ++ " is, so let’s expand it... This will keep going infinitely!"
                  , D.link "Hint"
                      "The root problem is often a typo in some variable name, but I recommend reading"
                      "bad-recursion"
                      "for more detailed advice, especially if you actually do need a recursive value."
                  ]
              )

          else
              (
                D.reflow $
                  "The `" <> N.toChars name <> "` definition is causing a very tricky infinite loop."
              ,
                D.stack
                  [ D.reflow $
                      "The `" <> N.toChars name
                      <> "` value depends on itself through the following chain of definitions:"
                  , D.cycle cycle D.fromName
                  , D.link "Hint"
                      "The root problem is often a typo in some variable name, but I recommend reading"
                      "bad-recursion"
                      "for more detailed advice, especially if you actually do want mutually recursive values."
                  ]
              )

    BadLetRec_Value (A.At region name) cycle ->
      Report.Report "CYCLIC VALUE" region [] $
        Code.toSnippet source region Nothing $
          if Graph.isSelfRecursive cycle
          then
              let
                makeTheory question details =
                  D.fillSep $ map (D.dullyellow . D.fromChars) (words question) ++ map D.fromChars (words details)
              in
                (
                  D.reflow $
                    "The `" <> N.toChars name <> "` value is defined directly in terms of itself, causing an infinite loop."
                ,
                  D.stack
                    [ makeTheory "Are you trying to mutate a variable?" $
                        "Elm does not have mutation, so when I see " ++ N.toChars name
                        ++ " defined in terms of " ++ N.toChars name
                        ++ ", I treat it as a recursive definition. Try giving the new value a new name!"
                    , makeTheory "Maybe you DO want a recursive value?" $
                        "To define " ++ N.toChars name ++ " we need to know what " ++ N.toChars name
                        ++ " is, so let’s expand it. Wait, but now we need to know what " ++ N.toChars name
                        ++ " is, so let’s expand it... This will keep going infinitely!"
                    , D.link "Hint"
                        "The root problem is often a typo in some variable name, but I recommend reading"
                        "bad-recursion"
                        "for more detailed advice, especially if you actually do need a recursive value."
                    ]
                )

          else
                (
                  D.reflow $
                    "I do not allow cyclic values in `let` expressions."
                ,
                  D.stack
                    [ D.reflow $
                        "The `" <> N.toChars name
                        <> "` value depends on itself through the following chain of definitions:"
                    , D.cycle cycle D.fromName
                    , D.link "Hint"
                        "The root problem is often a typo in some variable name, but I recommend reading"
                        "bad-recursion"
                        "for more detailed advice, especially if you actually do want mutually recursive values."
                    ]
                )

    Shadowing name r1 r2 ->
      Report.Report "SHADOWING" r2 [] $
        Code.toPair source r1 r2
          ( "These variables cannot have the same name:"
          , advice
          )
          ( D.reflow $ "The name `" <> N.toChars name <> "` is first defined here:"
          , "But then it is defined AGAIN over here:"
          , advice
          )
      where
        advice =
          D.stack
            [ D.reflow $
                "Think of a more helpful name for one of them and you should be all set!"
            , D.link "Note"
                "Linters advise against shadowing, so Elm makes “best practices” the default. Read"
                "shadowing"
                "for more details on this choice."
            ]

    TupleLargerThanThree region ->
      Report.Report "BAD TUPLE" region [] $
        Code.toSnippet source region Nothing
          (
            "I only accept tuples with two or three items. This has too many:"
          ,
            D.stack
              [ D.reflow $
                  "I recommend switching to records. Each item will be named, and you can use\
                  \ the `point.x` syntax to access them."

              , D.link "Note" "Read" "tuples"

                  "for more comprehensive advice on working with large chunks of data in Elm."
              ]
          )

    TypeVarsUnboundInUnion unionRegion typeName allVars unbound unbounds ->
      unboundTypeVars source unionRegion ["type"] typeName allVars unbound unbounds

    TypeVarsMessedUpInAlias aliasRegion typeName allVars unusedVars unboundVars ->
      case (unusedVars, unboundVars) of
        (unused:unuseds, []) ->
          let
            backQuoteType name = "`" <> D.fromType name <> "`"
            backQuoteVar  name = "`" <> D.fromVar  name <> "`"

            allUnusedNames =
              map A.toValue unusedVars

            (title, subRegion, overview, stuff) =
              case unuseds of
                [] ->
                  ("UNUSED TYPE VARIABLE"
                  , Just (A.toRegion unused)
                  , ["Type","alias",backQuoteType typeName,"does","not","use","the"
                    ,backQuoteVar (A.toValue unused),"type","variable."
                    ]
                  , [D.dullyellow (backQuoteVar (A.toValue unused))]
                  )

                _:_ ->
                  ( "UNUSED TYPE VARIABLES"
                  , Nothing
                  , ["Type","variables"]
                    ++ D.commaSep "and" id (map D.fromVar allUnusedNames)
                    ++ ["are","unused","in","the",backQuoteType typeName,"definition."]
                  , D.commaSep "and" D.dullyellow (map D.fromVar allUnusedNames)
                  )
          in
          Report.Report title aliasRegion [] $
            Code.toSnippet source aliasRegion subRegion
              (
                D.fillSep overview
              ,
                D.stack
                  [ D.fillSep $
                      ["I","recommend","removing"] ++ stuff ++ ["from","the","declaration,","like","this:"]
                  , D.indent 4 $ D.hsep $
                      ["type","alias",D.green (D.fromType typeName)]
                      ++ map D.fromVar (filter (`notElem` allUnusedNames) allVars)
                      ++ ["=", "..."]
                  , D.reflow $
                      "Why? Well, if I allowed `type alias Height a = Float` I would need to answer\
                      \ some weird questions. Is `Height Bool` the same as `Float`? Is `Height Bool`\
                      \ the same as `Height Int`? My solution is to not need to ask them!"
                  ]
              )

        ([], unbound:unbounds) ->
          unboundTypeVars source aliasRegion ["type","alias"] typeName allVars unbound unbounds

        (_, _) ->
          let
            unused  = map A.toValue unusedVars
            unbound = map A.toValue unboundVars

            theseAreUsed =
              case unbound of
                [x] ->
                  ["Type","variable",D.dullyellow ("`" <> D.fromVar x <> "`"),"appears"
                  ,"in","the","definition,","but","I","do","not","see","it","declared."
                  ]

                _ ->
                  ["Type","variables"]
                  ++ D.commaSep "and" D.dullyellow (map D.fromVar unbound)
                  ++ ["are","used","in","the","definition,","but","I","do","not","see","them","declared."]

            butTheseAreUnused =
              case unused of
                [x] ->
                  ["Likewise,","type","variable"
                  ,D.dullyellow ("`" <> D.fromVar x <> "`")
                  ,"is","delared,","but","not","used."
                  ]

                _ ->
                  ["Likewise,","type","variables"]
                  ++ D.commaSep "and" D.dullyellow (map D.fromVar unused)
                  ++ ["are","delared,","but","not","used."]

          in
          Report.Report "TYPE VARIABLE PROBLEMS" aliasRegion [] $
            Code.toSnippet source aliasRegion Nothing
              (
                D.reflow $
                  "Type alias `" <> T.nameToChars typeName <> "` has some type variable problems."
              ,
                D.stack
                  [ D.fillSep $ theseAreUsed ++ butTheseAreUnused
                  , D.reflow $
                      "My guess is that a definition like this will work better:"
                  , D.indent 4 $ D.hsep $
                      ["type", "alias", D.fromType typeName]
                      ++ map D.fromVar (filter (`notElem` unused) allVars)
                      ++ map (D.green . D.fromVar) unbound
                      ++ ["=", "..."]
                  ]
              )



-- BAD TYPE VARIABLES


unboundTypeVars :: Code.Source -> A.Region -> [D.Doc] -> T.Name -> [T.Var] -> (A.Located T.Var) -> [A.Located T.Var] -> Report.Report
unboundTypeVars source declRegion tipe typeName allVars (A.At varRegion unboundVar) unboundVars =
  let
    backQuoteType name = "`" <> D.fromType name <> "`"
    backQuoteVar  name = "`" <> D.fromVar  name <> "`"

    (title, subRegion, overview) =
      case map A.toValue unboundVars of
        [] ->
          ( "UNBOUND TYPE VARIABLE"
          , Just varRegion
          , ["The",backQuoteType typeName]
            ++ tipe
            ++ ["uses","an","unbound","type","variable",D.dullyellow (backQuoteVar unboundVar),"in","its","definition:"]
          )

        vars ->
          ( "UNBOUND TYPE VARIABLES"
          , Nothing
          , ["Type","variables"]
            ++ D.commaSep "and" D.dullyellow (D.fromVar unboundVar : map D.fromVar vars)
            ++ ["are","unbound","in","the",backQuoteType typeName] ++ tipe ++ ["definition:"]
          )
  in
  Report.Report title declRegion [] $
    Code.toSnippet source declRegion subRegion
      (
        D.fillSep overview
      ,
        D.stack
          [ D.reflow $
              "You probably need to change the declaration to something like this:"
          , D.indent 4 $ D.hsep $
              tipe
              ++ [D.fromType typeName]
              ++ map D.fromVar allVars
              ++ map (D.green . D.fromVar) (unboundVar : map A.toValue unboundVars)
              ++ ["=", "..."]
          , D.reflow $
              "Why? Well, imagine one `" ++ T.nameToChars typeName ++ "` where `" ++ T.varToChars unboundVar ++
              "` is an Int and another where it is a Bool. When we explicitly list the type\
              \ variables, the type checker can see that they are actually different types."
          ]
      )



-- IMPORT NOT FOUND


importNotFound :: Code.Source -> A.Region -> ModuleName.Canonical -> name -> [name] -> (name -> [Char]) -> Report.Report
importNotFound source region (ModuleName.Canonical _ home) name possibleNames toChars =
  let
    suggestions =
      List.map toChars $ List.take 4 $
        Suggest.sort (Module.toChars home) toChars possibleNames
  in
  Report.Report "BAD IMPORT" region suggestions $
    Code.toSnippet source region Nothing
      (
        D.reflow $
          "The `" <> Module.toChars home <> "` module does not expose `" <> toChars name <> "`:"
      ,
        case List.map D.fromChars suggestions of
          [] ->
            "I cannot find any super similar exposed names. Maybe it is private?"

          [alt] ->
            D.fillSep ["Maybe","you","want",D.dullyellow alt,"instead?"]

          alts ->
            D.stack
              [ "These names seem close though:"
              , D.indent 4 $ D.vcat $ List.map D.dullyellow alts
              ]
      )



-- NAME CLASH


nameClash :: Code.Source -> A.Region -> A.Region -> String -> Report.Report
nameClash source r1 r2 messageThatEndsWithPunctuation =
  Report.Report "NAME CLASH" r2 [] $
    Code.toPair source r1 r2
      (
        D.reflow messageThatEndsWithPunctuation
      ,
        "How can I know which one you want? Rename one of them!"
      )
      (
        D.reflow (messageThatEndsWithPunctuation <> " One here:")
      ,
        "And another one here:"
      ,
        "How can I know which one you want? Rename one of them!"
      )



-- AMBIGUOUS NAME


ambiguousName :: Code.Source -> A.Region -> Maybe Module.Prefix -> name -> Amb.Ambiguous Module.Name -> String -> (name -> String) -> Report.Report
ambiguousName source region maybePrefix name homes thing toChars =
  let
    possibleHomes = List.sort (Amb.toList homes)
  in
  Report.Report "AMBIGUOUS NAME" region [] $
    Code.toSnippet source region Nothing $
      case maybePrefix of
        Nothing ->
          let
            homeToYellowDoc home =
              D.dullyellow (D.fromModule home <> "." <> D.fromChars (toChars name))
          in
          (
            D.reflow $ "This usage of `" ++ toChars name ++ "` is ambiguous:"
          ,
            D.stack
              [ D.reflow $
                  "This name is exposed by " ++ show (length possibleHomes) ++ " of your imports, so I am not\
                  \ sure which one to use:"
              , D.indent 4 $ D.vcat $ map homeToYellowDoc possibleHomes
              , D.reflow $
                  "I recommend using qualified names for imported values. I also recommend having\
                  \ at most one `exposing (..)` per file to make name clashes like this less common\
                  \ in the long run."
              , D.link "Note" "Check out" "imports" "for more info on the import syntax."
              ]
          )

        Just prefix ->
          let
            homeToYellowDoc home =
              if prefix == Module.toPrefix home
              then D.cyan "import" <+> D.fromModule home
              else D.cyan "import" <+> D.fromModule home <+> D.cyan "as" <+> D.fromPrefix prefix

            eitherOrAny =
              if length possibleHomes == 2 then "either" else "any"
          in
          (
            D.reflow $ "This usage of `" ++ Module.prefixToChars prefix ++ "." ++ toChars name ++ "` is ambiguous."
          ,
            D.stack
              [ D.reflow $
                  "It could refer to a " ++ thing ++ " from "
                  ++ eitherOrAny ++ " of these imports:"
              , D.indent 4 $ D.vcat $ map homeToYellowDoc possibleHomes
              , D.reflowLink "Read" "imports" "to learn how to clarify which one you want."
              ]
          )



-- BAD ARITY


badArity :: Code.Source -> A.Region -> name -> Int -> Int -> (name -> String) -> String -> Report.Report
badArity source region name expected actual toChars thing =
  if actual < expected then
    Report.Report "TOO FEW ARGS" region [] $
      Code.toSnippet source region Nothing
        (
          D.reflow $
            "The `" <> toChars name <> "` " <> thing <> " needs "
            <> D.args expected <> ", but I see " <> show actual <> " instead:"
        ,
          D.reflow $
            "What is missing? Are some parentheses misplaced?"
        )

  else
    Report.Report "TOO MANY ARGS" region [] $
      Code.toSnippet source region Nothing
        (
          D.reflow $
            "The `" <> toChars name <> "` " <> thing <> " needs "
            <> D.args expected <> ", but I see " <> show actual <> " instead:"
        ,
          if actual - expected == 1
          then "Which is the extra one? Maybe some parentheses are missing?"
          else "Which are the extra ones? Maybe some parentheses are missing?"
        )



-- NOT FOUND


notFound :: Code.Source -> A.Region -> Maybe Module.Prefix -> name -> String -> PossibleNames name -> (name -> String) -> Report.Report
notFound source region maybePrefix name thing (PossibleNames locals quals) toChars =
  let
    givenName =
      maybe toChars toQualString maybePrefix name

    possibleNames =
      let
        addQuals prefix localSet allNames =
          Set.foldr (\x xs -> toQualString prefix x : xs) allNames localSet
      in
      Map.foldrWithKey addQuals (map toChars (Set.toList locals)) quals

    toQualString h n =
      Module.prefixToChars h ++ "." ++ toChars n

    nearbyNames =
      take 4 (Suggest.sort givenName id possibleNames)

    toDetails noSuggestionDetails yesSuggestionDetails =
      case nearbyNames of
        [] ->
          D.stack
            [ D.reflow noSuggestionDetails
            , D.link "Hint" "Read" "imports" "to see how `import` declarations work in Elm."
            ]

        suggestions ->
          D.stack
            [ D.reflow yesSuggestionDetails
            , D.indent 4 $ D.vcat $ map D.dullyellow $ map D.fromChars suggestions
            , D.link "Hint" "Read" "imports" "to see how `import` declarations work in Elm."
            ]

  in
  Report.Report "NAMING ERROR" region nearbyNames $
    Code.toSnippet source region Nothing
      (
        D.reflow $
          "I cannot find a `" ++ givenName ++ "` " ++ thing ++ ":"
      ,
        case maybePrefix of
          Nothing ->
            toDetails
              "Is there an `import` or `exposing` missing up top?"
              "These names seem close though:"

          Just prefix ->
            case Map.lookup prefix quals of
              Nothing ->
                toDetails
                  ("I cannot find a `" ++ Module.prefixToChars prefix ++ "` module. Is there an `import` for it?")
                  ("I cannot find a `" ++ Module.prefixToChars prefix ++ "` import. These names seem close though:")

              Just _ ->
                toDetails
                  ("The `" ++ Module.prefixToChars prefix ++ "` module does not expose a `" ++ toChars name ++ "` " ++ thing ++ ".")
                  ("The `" ++ Module.prefixToChars prefix ++ "` module does not expose a `" ++ toChars name ++ "` " ++ thing ++ ". These names seem close though:")
      )



{-- VAR ERROR


varErrorToReport :: VarError -> Report.Report
varErrorToReport (VarError kind name problem suggestions) =
  let
    learnMore orMaybe =
      D.reflow $
        orMaybe <> " `import` works different than you expect? Learn all about it here: "
        <> D.hintLink "imports"

    namingError overview maybeStarter specializedSuggestions =
      Report.reportDoc "NAMING ERROR" Nothing overview $
        case D.maybeYouWant' maybeStarter specializedSuggestions of
          Nothing ->
            learnMore "Maybe"
          Just doc ->
            D.stack [ doc, learnMore "Or maybe" ]

    specialNamingError specialHint =
      Report.reportDoc "NAMING ERROR" Nothing (cannotFind kind name) (D.hsep specialHint)
  in
  case problem of
    Ambiguous ->
      namingError (ambiguous kind name) Nothing suggestions

    UnknownQualifier qualifier localName ->
      namingError
        (cannotFind kind name)
        (Just $ text $ "No module called `" <> qualifier <> "` has been imported.")
        (map (\modul -> modul <> "." <> localName) suggestions)

    QualifiedUnknown qualifier localName ->
      namingError
        (cannotFind kind name)
        (Just $ text $ "`" <> qualifier <> "` does not expose `" <> localName <> "`.")
        (map (\v -> qualifier <> "." <> v) suggestions)

    ExposedUnknown ->
      case name of
        "!="  -> specialNamingError (notEqualsHint name)
        "!==" -> specialNamingError (notEqualsHint name)
        "===" -> specialNamingError equalsHint
        "%"   -> specialNamingError modHint
        _     -> namingError (cannotFind kind name) Nothing suggestions


cannotFind :: VarKind -> Text -> [Doc]
cannotFind kind rawName =
  let ( a, thing, name ) = toKindInfo kind rawName in
  [ "Cannot", "find", a, thing, "named", D.dullyellow name <> ":" ]


ambiguous :: VarKind -> Text -> [Doc]
ambiguous kind rawName =
  let ( _a, thing, name ) = toKindInfo kind rawName in
  [ "This", "usage", "of", "the", D.dullyellow name, thing, "is", "ambiguous." ]


notEqualsHint :: Text -> [Doc]
notEqualsHint op =
  [ "Looking", "for", "the", "“not", "equal”", "operator?", "The", "traditional"
  , D.dullyellow $ text $ "(" <> op <> ")"
  , "is", "replaced", "by", D.green "(/=)", "in", "Elm.", "It", "is", "meant"
  , "to", "look", "like", "the", "“not", "equal”", "sign", "from", "math!", "(≠)"
  ]


equalsHint :: [Doc]
equalsHint =
  [ "A", "special", D.dullyellow "(===)", "operator", "is", "not", "needed"
  , "in", "Elm.", "We", "use", D.green "(==)", "for", "everything!"
  ]


modHint :: [Doc]
modHint =
  [ "Rather", "than", "a", D.dullyellow "(%)", "operator,"
  , "Elm", "has", "a", D.green "modBy", "function."
  , "Learn", "more", "here:"
  , "<https://package.elm-lang.org/packages/elm/core/latest/Basics#modBy>"
  ]


-}


-- ARG MISMATCH


_argMismatchReport :: Code.Source -> A.Region -> String -> N.Name -> Int -> Int -> Report.Report
_argMismatchReport source region kind name expected actual =
  let
    numArgs =
      "too "
      <> (if actual < expected then "few" else "many")
      <> " arguments"
  in
    Report.Report (map Char.toUpper numArgs) region [] $
      Code.toSnippet source region Nothing
        (
          D.reflow $
            kind <> " " <> N.toChars name <> " has " <> numArgs <> "."
        ,
          D.reflow $
            "Expecting " <> show expected <> ", but got " <> show actual <> "."
        )



-- BAD ALIAS RECURSION


aliasRecursionReport :: Code.Source -> A.Located T.Name -> [T.Var] -> Src.Type -> Graph.MinimalCycle T.Name -> Report.Report
aliasRecursionReport source (A.At region name) args tipe cycle =
  if Graph.isSelfRecursive cycle
  then
      Report.Report "ALIAS PROBLEM" region [] $
        Code.toSnippet source region Nothing
          (
            "This type alias is recursive, forming an infinite type!"
          ,
            D.stack
              [ D.reflow $
                  "When I expand a recursive type alias, it just keeps getting bigger and bigger.\
                  \ So dealiasing results in an infinitely large type! Try this instead:"
              , D.indent 4 $
                  aliasToUnionDoc name args tipe
              , D.link "Hint"
                  "This is kind of a subtle distinction. I suggested the naive fix, but I recommend reading"
                  "recursive-alias"
                  "for ideas on how to do better."
              ]
          )
  else
      Report.Report "ALIAS PROBLEM" region [] $
        Code.toSnippet source region Nothing
          (
            "This type alias is part of a mutually recursive set of type aliases."
          ,
            D.stack
              [ "It is part of this cycle of type aliases:"
              , D.cycle cycle D.fromType
              , D.reflow $
                  "You need to convert at least one of these type aliases into a `type`."
              , D.link "Note" "Read" "recursive-alias"
                  "to learn why this `type` vs `type alias` distinction matters. It is subtle but important!"
              ]
          )


aliasToUnionDoc :: T.Name -> [T.Var] -> Src.Type -> Doc
aliasToUnionDoc name args tipe =
  D.vcat
    [ D.dullyellow $
        "type" <+> D.fromType name <+> (foldr (<+>) "=" (map D.fromVar args))
    , D.green $
        D.indent 4 (D.fromType name)
    , D.dullyellow $
        D.indent 8 (RT.srcToDoc RT.App tipe)
    ]
