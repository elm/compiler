{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Reporting.Error.Canonicalize
  ( Error(..)
  , BadArityContext(..)
  , InvalidPayload(..)
  , PortProblem(..)
  , DuplicatePatternContext(..)
  , PossibleNames(..)
  , VarKind(..)
  , toReport
  )
  where


import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified AST.Canonical as Can
import qualified AST.Source as Src
import qualified AST.Module.Name as ModuleName
import qualified Data.Index as Index
import qualified Elm.Name as N
import qualified Reporting.Annotation as A
import qualified Reporting.Region as R
import qualified Reporting.Render.Code as Code
import qualified Reporting.Render.Type as RT
import qualified Reporting.Report as Report
import qualified Reporting.Helpers as H
import Reporting.Helpers ( Doc, (<+>), (<>) )



-- CANONICALIZATION ERRORS


data Error
  = AnnotationTooShort R.Region N.Name Index.ZeroBased Int
  | AmbiguousVar R.Region (Maybe N.Name) N.Name [ModuleName.Canonical]
  | AmbiguousType R.Region (Maybe N.Name) N.Name [ModuleName.Canonical]
  | AmbiguousCtor R.Region (Maybe N.Name) N.Name [ModuleName.Canonical]
  | AmbiguousBinop R.Region N.Name [ModuleName.Canonical]
  | BadArity R.Region BadArityContext N.Name Int Int
  | Binop R.Region N.Name N.Name
  | DuplicateDecl N.Name R.Region R.Region
  | DuplicateType N.Name R.Region R.Region
  | DuplicateCtor N.Name R.Region R.Region
  | DuplicateBinop N.Name R.Region R.Region
  | DuplicateField N.Name R.Region R.Region
  | DuplicateAliasArg N.Name N.Name R.Region R.Region
  | DuplicateUnionArg N.Name N.Name R.Region R.Region
  | DuplicatePattern DuplicatePatternContext N.Name R.Region R.Region
  | EffectNotFound R.Region N.Name
  | EffectFunctionNotFound R.Region N.Name
  | ExportDuplicate N.Name R.Region R.Region
  | ExportNotFound R.Region VarKind N.Name [N.Name]
  | ExportOpenAlias R.Region N.Name
  | ImportCtorByName R.Region N.Name N.Name
  | ImportNotFound R.Region N.Name [ModuleName.Canonical]
  | ImportOpenAlias R.Region N.Name
  | ImportExposingNotFound R.Region ModuleName.Canonical N.Name [N.Name]
  | NotFoundVar R.Region (Maybe N.Name) N.Name PossibleNames
  | NotFoundType R.Region (Maybe N.Name) N.Name PossibleNames
  | NotFoundCtor R.Region (Maybe N.Name) N.Name PossibleNames
  | NotFoundBinop R.Region N.Name (Set.Set N.Name)
  | PatternHasRecordCtor R.Region N.Name
  | PortPayloadInvalid R.Region N.Name Can.Type InvalidPayload
  | PortTypeInvalid R.Region N.Name PortProblem
  | RecursiveAlias R.Region N.Name [N.Name] Src.Type [N.Name]
  | RecursiveDecl [Can.Def]
  | RecursiveLet (A.Located N.Name) [N.Name]
  | Shadowing N.Name R.Region R.Region
  | TupleLargerThanThree R.Region
  | TypeVarsUnboundInUnion R.Region N.Name [N.Name] (N.Name, R.Region) [(N.Name, R.Region)]
  | TypeVarsMessedUpInAlias R.Region N.Name [N.Name] [(N.Name, R.Region)] [(N.Name, R.Region)]


data BadArityContext
  = TypeArity
  | PatternArity


data DuplicatePatternContext
  = DPLambdaArgs
  | DPFuncArgs N.Name
  | DPCaseBranch
  | DPLetBinding
  | DPDestruct


data InvalidPayload
  = ExtendedRecord
  | Function
  | TypeVariable N.Name
  | UnsupportedType N.Name


data PortProblem
  = CmdNoArg
  | CmdExtraArgs Int
  | CmdBadMsg
  | SubBad
  | NotCmdOrSub


data PossibleNames =
  PossibleNames
    { _locals :: Set.Set N.Name
    , _quals :: Map.Map N.Name (Set.Set N.Name)
    }



-- KIND


data VarKind
  = BadOp
  | BadVar
  | BadPattern
  | BadType


toKindInfo :: VarKind -> N.Name -> ( Doc, Doc, Doc )
toKindInfo kind name =
  case kind of
    BadOp ->
      ( "an", "operator", "(" <> H.nameToDoc name <> ")" )

    BadVar ->
      ( "a", "value", "`" <> H.nameToDoc name <> "`" )

    BadPattern ->
      ( "a", "pattern", "`" <> H.nameToDoc name <> "`" )

    BadType ->
      ( "a", "type", "`" <> H.nameToDoc name <> "`" )



-- TO REPORT


toReport :: Code.Source -> Error -> Report.Report
toReport source err =
  case err of
    AnnotationTooShort region name index leftovers ->
      let
        numTypeArgs = Index.toHuman index
        numDefArgs = numTypeArgs + leftovers
      in
      Report.Report "BAD TYPE ANNOTATION" region [] $
        Report.toCodeSnippet source region Nothing
          (
            H.reflow $
              "The type annotation for `" <> N.toString name <> "` says it can accept "
              <> H.args numTypeArgs <> ", but the definition says it has "
              <> H.args numDefArgs <> ":"
          ,
            H.reflow $
              "Is the type annotation missing something? Should some argument"
              <> (if leftovers == 1 then "" else "s")
              <> " be deleted? Maybe some parentheses are missing?"
          )

    AmbiguousVar region maybePrefix name possibleHomes ->
      ambiguousName source region maybePrefix name possibleHomes "variable"

    AmbiguousType region maybePrefix name possibleHomes ->
      ambiguousName source region maybePrefix name possibleHomes "type"

    AmbiguousCtor region maybePrefix name possibleHomes ->
      ambiguousName source region maybePrefix name possibleHomes "constructor"

    AmbiguousBinop region name possibleHomes ->
      ambiguousName source region Nothing name possibleHomes "operator"

    BadArity region badArityContext name expected actual ->
      let
        thing =
          case badArityContext of
            TypeArity    -> "type"
            PatternArity -> "constructor"
      in
      if actual < expected then
        Report.Report "TOO FEW ARGS" region [] $
          Report.toCodeSnippet source region Nothing
            (
              H.reflow $
                "The `" <> N.toString name <> "` " <> thing <> " needs "
                <> H.args expected <> ", but I only see " <> show actual <> ":"
            ,
              "What is missing? Is there some typo?"
            )

      else
        Report.Report "TOO MANY ARGS" region [] $
          Report.toCodeSnippet source region Nothing
            (
              H.reflow $
                "The `" <> N.toString name <> "` " <> thing <> " needs "
                <> H.args expected <> ", but I see " <> show actual <> " instead:"
            ,
              if actual - expected == 1 then
                "Which is the extra one? Maybe some parentheses are missing?"
              else
                "Which are the extra ones? Maybe some parentheses are missing?"
            )

    Binop region op1 op2 ->
      Report.Report "INFIX PROBLEM" region [] $
        Report.toCodeSnippet source region Nothing
          (
            H.reflow $
              "You cannot mix (" <> N.toString op1 <> ") and (" <> N.toString op2 <> ") without parentheses."
          ,
            H.reflow
              "I do not know how to group these expressions. Add parentheses for me!"
          )

    DuplicateDecl name r1 r2 ->
      nameClash source r1 r2 $
        "This file has multiple `" <> N.toString name <> "` declarations."

    DuplicateType name r1 r2 ->
      nameClash source r1 r2 $
        "This file defines multiple `" <> N.toString name <> "` types."

    DuplicateCtor name r1 r2 ->
      nameClash source r1 r2 $
        "This file defines multiple `" <> N.toString name <> "` type constructors."

    DuplicateBinop name r1 r2 ->
      nameClash source r1 r2 $
        "This file defines multiple (" <> N.toString name <> ") operators."

    DuplicateField name r1 r2 ->
      nameClash source r1 r2 $
        "This record has multiple `" <> N.toString name <> "` fields."

    DuplicateAliasArg typeName name r1 r2 ->
      nameClash source r1 r2 $
        "The `" <> N.toString typeName <> "` type alias has multilpe `" <> N.toString name <> "` type variables."

    DuplicateUnionArg typeName name r1 r2 ->
      nameClash source r1 r2 $
        "The `" <> N.toString typeName <> "` type has multilpe `" <> N.toString name <> "` type variables."

    DuplicatePattern context name r1 r2 ->
      nameClash source r1 r2 $
        case context of
          DPLambdaArgs ->
            "This anonymous function has multiple `" <> N.toString name <> "` arguments."

          DPFuncArgs funcName ->
            "The `" <> N.toString funcName <> "` function has multiple `" <> N.toString name <> "` arguments."

          DPCaseBranch ->
            "This `case` pattern has multiple `" <> N.toString name <> "` variables."

          DPLetBinding ->
            "This `let` expression defines `" <> N.toString name <> "` more than once!"

          DPDestruct ->
            "This pattern contains multiple `" <> N.toString name <> "` variables."

    EffectNotFound region name ->
      Report.Report "EFFECT PROBLEM" region [] $
        Report.toCodeSnippet source region Nothing
          (
            H.reflow $
              "You have declared that `" ++ N.toString name ++ "` is an effect type:"
          ,
            H.reflow $
              "But I cannot find a union type named `" ++ N.toString name ++ "` in this file!"
          )

    EffectFunctionNotFound region name ->
      Report.Report "EFFECT PROBLEM" region [] $
        Report.toCodeSnippet source region Nothing
          (
            H.reflow $
              "This kind of effect module must define a `" ++ N.toString name ++ "` function."
          ,
            H.reflow $
              "But I cannot find `" ++ N.toString name ++ "` in this file!"
          )


    ExportDuplicate name r1 r2 ->
      let
        messageThatEndsWithPunctuation =
          "You are trying to expose `" <> N.toString name <> "` multiple times!"
      in
      Report.Report "REDUNDANT EXPORT" r2 [] $
        Report.toCodePair source r1 r2
          (
            H.reflow messageThatEndsWithPunctuation
          ,
            "Remove one of them and you should be all set!"
          )
          (
            H.reflow (messageThatEndsWithPunctuation <> " Once here:")
          ,
            "And again right here:"
          ,
            "Remove one of them and you should be all set!"
          )

    ExportNotFound region kind rawName possibleNames ->
      let
        suggestions =
          map N.toString $ take 4 $
            H.nearbyNames N.toString rawName possibleNames
      in
      Report.Report "UNKNOWN EXPORT" region suggestions $
        let (a, thing, name) = toKindInfo kind rawName in
        H.stack
          [ H.fillSep
              ["You","are","trying","to","expose",a,thing,"named"
              ,name,"but","I","cannot","find","its","definition."
              ]
          , case map H.text suggestions of
              [] ->
                H.reflow $
                  "I do not see any super similar names in this file. Is the definition missing?"

              [alt] ->
                H.fillSep ["Maybe","you","want",H.dullyellow alt,"instead?"]

              alts ->
                H.stack
                  [ "These names seem close though:"
                  , H.indent 4 $ H.vcat $ map H.dullyellow alts
                  ]
          ]

    ExportOpenAlias region name ->
      Report.Report "BAD EXPORT" region [] $
        Report.toCodeSnippet source region Nothing
          (
            H.reflow $
              "The (..) syntax is for exposing union type constructors. It cannot be used with a type alias like `"
              ++ N.toString name ++ "` though."
          ,
            H.reflow $
              "Remove the (..) and you should be fine!"
          )

    ImportCtorByName region ctor tipe ->
      Report.Report "BAD IMPORT" region [] $
        Report.toCodeSnippet source region Nothing
          (
            H.reflow $
              "You are trying to import the `" <> N.toString ctor
              <> "` type constructor by name:"
          ,
            H.fillSep
              ["Try","importing",H.green (H.nameToDoc tipe <> "(..)"),"instead."
              ,"The","dots","mean","“expose","the",H.nameToDoc tipe,"type","and"
              ,"all","its","constructors”","so","it","gives","you","access","to"
              , H.nameToDoc ctor <> "."
              ]
          )

    ImportNotFound region name _ ->
      --
      -- NOTE: this should always be detected by `builder`
      -- So this error should never actually get printed out.
      --
      Report.Report "UNKNOWN IMPORT" region [] $
        Report.toCodeSnippet source region Nothing
          (
            H.reflow $
              "I could not find a `" <> N.toString name <> "` module to import!"
          ,
            mempty
          )

    ImportOpenAlias region name ->
      Report.Report "BAD IMPORT" region [] $
        Report.toCodeSnippet source region Nothing
          (
            H.reflow $
              "The `" <> N.toString name <> "` type alias cannot be followed by (..) like this:"
          ,
            H.stack
              [ "Remove the (..) and it should work."
              , H.link "Hint"
                  "The distinction between `type` and `type alias` is important here. Read"
                  "types-vs-type-aliases"
                  "to learn more."
              ]
          )

    ImportExposingNotFound region (ModuleName.Canonical _ home) value possibleNames ->
      let
        suggestions =
          map N.toString $ take 4 $
            H.nearbyNames N.toString home possibleNames
      in
      Report.Report "BAD IMPORT" region suggestions $
        Report.toCodeSnippet source region Nothing
          (
            H.reflow $
              "The `" <> N.toString home
              <> "` module does not expose `"
              <> N.toString value <> "`:"
          ,
            case map H.text suggestions of
              [] ->
                "I cannot find any super similar exposed names. Maybe it is private?"

              [alt] ->
                H.fillSep ["Maybe","you","want",H.dullyellow alt,"instead?"]

              alts ->
                H.stack
                  [ "These names seem close though:"
                  , H.indent 4 $ H.vcat $ map H.dullyellow alts
                  ]
          )

    NotFoundVar region prefix name possibleNames ->
      notFound source region prefix name "variable" possibleNames

    NotFoundType region prefix name possibleNames ->
      notFound source region prefix name "type" possibleNames

    NotFoundCtor region prefix name possibleNames ->
      notFound source region prefix name "constructor" possibleNames

    NotFoundBinop region op locals ->
      if op == "===" then
        Report.Report "UNKNOWN OPERATOR" region ["=="] $
          Report.toCodeSnippet source region Nothing
            (
              "Elm does not have a (===) operator like JavaScript."
            ,
              "Switch to (==) instead."
            )

      else if op == "!=" || op == "!==" then
        Report.Report "UNKNOWN OPERATOR" region ["/="] $
          Report.toCodeSnippet source region Nothing
            (
              H.reflow $
                "Elm uses a different name for the “not equal” operator:"
            ,
              H.stack
                [ H.reflow "Switch to (/=) instead."
                , H.toSimpleNote $
                    "Our (/=) operator is supposed to look like a real “not equal” sign (≠). I hope that history will remember ("
                    ++ N.toString op ++ ") as a werid and temporary choice."
                ]
            )

      else if op == "**" then
        Report.Report "UNKNOWN OPERATOR" region ["^","*"] $
          Report.toCodeSnippet source region Nothing
            (
              H.reflow $
                "I do not recognize this (**) operator:"
            ,
              H.reflow $
                "Switch to (^) for exponentiation. Or switch to (*) for multiplication."
            )

      else
        let
          suggestions =
            map N.toString $ take 2 $
              H.nearbyNames N.toString op (Set.toList locals)

          format altOp =
            H.dullyellow $ "(" <> altOp <> ")"
        in
        Report.Report "UNKNOWN OPERATOR" region suggestions $
          Report.toCodeSnippet source region Nothing
            (
              H.reflow $
                "I do not recognize this (" ++ N.toString op ++ ") operator."
            ,
              H.fillSep $
                ["Is","there","an","`import`","and","`exposing`","entry","for","it?"]
                ++
                  case map H.text suggestions of
                    [] ->
                      []

                    alts ->
                      ["Maybe","you","want"] ++ H.commaSep "or" format alts ++ ["instead?"]
            )

    PatternHasRecordCtor region name ->
      Report.Report "BAD PATTERN" region [] $
        Report.toCodeSnippet source region Nothing
          (
            H.reflow $
              "You can construct records by using `" <> N.toString name
              <> "` as a function, but it is not available in pattern matching like this:"
          ,
            H.reflow $
              "I recommend matching matching the record as a variable and unpacking it later."
          )

    PortPayloadInvalid region portName _badType invalidPayload ->
      let
        formatDetails (aBadKindOfThing, elaboration) =
          Report.Report "PORT ERROR" region [] $
            Report.toCodeSnippet source region Nothing
              (
                H.reflow $
                  "The `" <> N.toString portName <> "` port is trying to transmit " <> aBadKindOfThing <> ":"
              ,
                H.stack
                  [ elaboration
                  , H.link "Hint"
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
              H.reflow $
                "But the exact shape of the record must be known at compile time. No type variables!"
            )

          Function ->
            (
              "a function"
            ,
              H.reflow $
                "But functions cannot be sent in and out ports. If we allowed functions in from JS\
                \ they may perform some side-effects. If we let functions out, they could produce\
                \ incorrect results because Elm optimizations assume there are no side-effects."
            )


          TypeVariable name ->
            (
              "an unspecified type"
            ,
              H.reflow $
                "But type variables like `" <> N.toString name <> "` cannot flow through ports.\
                \ I need to know exactly what type of data I am getting, so I can guarantee that\
                \ unexpected data cannot sneak in and crash the Elm program."
            )

          UnsupportedType name ->
            (
              "a `" <> N.toString name <> "` value"
            ,
              H.stack
                [ H.reflow $ "I cannot handle that. The types that CAN flow in and out of Elm include:"
                , H.indent 4 $
                    H.reflow $
                      "Ints, Floats, Bools, Strings, Maybes, Lists, Arrays,\
                      \ tuples, records, and JSON values."
                , H.reflow $
                    "Since JSON values can flow through, you can use JSON encoders and decoders\
                    \ to allow other types through as well. More advanced users often just do\
                    \ everything with encoders and decoders for more control and better errors."
                ]
            )

    PortTypeInvalid region name portProblem ->
      let
        formatDetails (before, after) =
          Report.Report "BAD PORT" region [] $
            Report.toCodeSnippet source region Nothing $
              (
                H.reflow before
              ,
                H.stack
                  [ after
                  , H.link "Hint" "Read" "ports"
                      "for more advice. For example, do not end up with one port per JS function!"
                  ]
              )
      in
      formatDetails $
        case portProblem of
          CmdNoArg ->
            (
              "The `" <> N.toString name <> "` port cannot be just a command."
            ,
              H.reflow $
                "It can be (() -> Cmd msg) if you just need to trigger a JavaScript\
                \ function, but there is often a better way to set things up."
            )

          CmdExtraArgs n ->
            (
              "The `" <> N.toString name <> "` port can only send ONE value out to JavaScript."
            ,
              let
                theseItemsInSomething
                  | n == 2 = "both of these items into a tuple or record"
                  | n == 3 = "these " ++ show n ++ " items into a tuple or record"
                  | True   = "these " ++ show n ++ " items into a record"
              in
              H.reflow $
                "You can put " ++ theseItemsInSomething ++ " to send them out though."
            )

          CmdBadMsg ->
            (
              "The `" <> N.toString name <> "` port cannot send any messages to the `update` function."
            ,
              H.reflow $
                "It must produce a (Cmd msg) type. Notice the lower case `msg` type\
                \ variable. The command will trigger some JS code, but it will not send\
                \ anything particular back to Elm."
            )

          SubBad ->
            ( "There is something off about this `" <> N.toString name <> "` port declaration."
            ,
              H.stack
                [ H.reflow $
                    "To receive messages from JavaScript, you need to define a port like this:"
                , H.indent 4 $ H.dullyellow $ H.text $
                    "port " <> N.toString name <> " : (Int -> msg) -> Sub msg"
                , H.reflow $
                    "Now every time JS sends an `Int` to this port, it is converted to a `msg`.\
                    \ And if you subscribe, those `msg` values will be piped into your `update`\
                    \ function. The only thing you can customize here is the `Int` type."
                ]
            )

          NotCmdOrSub ->
            (
              "I am confused about the `" <> N.toString name <> "` port declaration."
            ,
              H.reflow $
                "Ports need to produce a command (Cmd) or a subscription (Sub) but\
                \ this is neither. I do not know how to handle this."
            )

    RecursiveAlias region name args tipe others ->
        aliasRecursionReport source region name args tipe others

    RecursiveDecl cyclicValueDefs ->
      let
        toName def =
          case def of
            Can.Def name _ _ -> name
            Can.TypedDef name _ _ _ _ -> name

        makeTheory question details =
          H.fillSep $ map (H.dullyellow . H.text) (words question) ++ map H.text (words details)
      in
      case map toName cyclicValueDefs of
        [] ->
          error
            "There is some compiler bug in reporting cyclic definitions.\n\
            \Please get a <http://sscce.org/> and share the details at\n\
            \<https://github.com/elm-lang/elm-compiler/issues>"

        A.At region name : otherNames ->
          Report.Report "CYCLIC DEFINITION" region [] $
            Report.toCodeSnippet source region Nothing $
              case map A.toValue otherNames of
                [] ->
                  (
                    H.reflow $
                      "The `" <> N.toString name <> "` value is defined directly in terms of itself, causing an infinite loop."
                  ,
                    H.stack
                      [ makeTheory "Are you are trying to mutate a variable?" $
                          "Elm does not have mutation, so when I see " ++ N.toString name
                          ++ " defined in terms of " ++ N.toString name
                          ++ ", I treat it as a recursive definition. Try giving the new value a new name!"
                      , makeTheory "Maybe you DO want a recursive value?" $
                          "To define " ++ N.toString name ++ " we need to know what " ++ N.toString name
                          ++ " is, so let’s expand it. Wait, but now we need to know what " ++ N.toString name
                          ++ " is, so let’s expand it... This will keep going infinitely!"
                      , H.link "Hint"
                          "The root problem is often a typo in some variable name, but I recommend reading"
                          "bad-recursion"
                          "for more detailed advice, especially if you actually do need a recursive value."
                      ]
                  )

                names ->
                  (
                    H.reflow $
                      "The `" <> N.toString name <> "` definition is causing a very tricky infinite loop."
                  ,
                    H.stack
                      [ H.reflow $
                          "The `" <> N.toString name
                          <> "` value depends on itself through the following chain of definitions:"
                      , H.indent 4 $ H.drawCycle (name:names)
                      , H.link "Hint"
                          "The root problem is often a typo in some variable name, but I recommend reading"
                          "bad-recursion"
                          "for more detailed advice, especially if you actually do want mutually recursive values."
                      ]
                  )

    RecursiveLet (A.At region name) names ->
      Report.Report "CYCLIC VALUE" region [] $
        Report.toCodeSnippet source region Nothing $
          case names of
            [] ->
              let
                makeTheory question details =
                  H.fillSep $ map (H.dullyellow . H.text) (words question) ++ map H.text (words details)
              in
                (
                  H.reflow $
                    "The `" <> N.toString name <> "` value is defined directly in terms of itself, causing an infinite loop."
                ,
                  H.stack
                    [ makeTheory "Are you are trying to mutate a variable?" $
                        "Elm does not have mutation, so when I see " ++ N.toString name
                        ++ " defined in terms of " ++ N.toString name
                        ++ ", I treat it as a recursive definition. Try giving the new value a new name!"
                    , makeTheory "Maybe you DO want a recursive value?" $
                        "To define " ++ N.toString name ++ " we need to know what " ++ N.toString name
                        ++ " is, so let’s expand it. Wait, but now we need to know what " ++ N.toString name
                        ++ " is, so let’s expand it... This will keep going infinitely!"
                    , H.link "Hint"
                        "The root problem is often a typo in some variable name, but I recommend reading"
                        "bad-recursion"
                        "for more detailed advice, especially if you actually do need a recursive value."
                    ]
                )

            _ ->
                (
                  H.reflow $
                    "I do not allow cyclic values in `let` expressions."
                ,
                  H.stack
                    [ H.reflow $
                        "The `" <> N.toString name
                        <> "` value depends on itself through the following chain of definitions:"
                    , H.indent 4 $ H.drawCycle (name:names)
                    , H.link "Hint"
                        "The root problem is often a typo in some variable name, but I recommend reading"
                        "bad-recursion"
                        "for more detailed advice, especially if you actually do want mutually recursive values."
                    ]
                )

    Shadowing name r1 r2 ->
      Report.Report "SHADOWING" r2 [] $
        Report.toCodePair source r1 r2
          ( "These variables cannot have the same name:"
          , advice
          )
          ( H.reflow $ "The name `" <> N.toString name <> "` is first defined here:"
          , "But then it is defined AGAIN over here:"
          , advice
          )
      where
        advice =
          H.stack
            [ H.reflow $
                "Think of a more helpful name for one of them and you should be all set!"
            , H.link "Note"
                "Linters advise against shadowing, so Elm makes “best practices” the default. Read"
                "shadowing"
                "for more details on this choice."
            ]

    TupleLargerThanThree region ->
      Report.Report "BAD TUPLE" region [] $
        Report.toCodeSnippet source region Nothing
          (
            "I only accept tuples with two or three items. This has too many:"
          ,
            H.stack
              [ H.reflow $
                  "I recommend switching to records. Each item will be named, and you can use\
                  \ the `point.x` syntax to access them."

              , H.link "Note" "Read" "tuples"

                  "for more comprehensive advice on working with large chunks of data in Elm."
              ]
          )

    TypeVarsUnboundInUnion unionRegion typeName allVars unbound unbounds ->
      unboundTypeVars source unionRegion ["type"] typeName allVars unbound unbounds

    TypeVarsMessedUpInAlias aliasRegion typeName allVars unusedVars unboundVars ->
      case (unusedVars, unboundVars) of
        (unused:unuseds, []) ->
          let
            backQuote name =
              "`" <> H.nameToDoc name <> "`"

            allUnusedNames =
              map fst unusedVars

            (title, subRegion, overview, stuff) =
              case unuseds of
                [] ->
                  ("UNUSED TYPE VARIABLE"
                  , Just (snd unused)
                  , ["Type","alias",backQuote typeName,"does","not","use","the"
                    ,backQuote (fst unused),"type","variable."
                    ]
                  , [H.dullyellow (backQuote (fst unused))]
                  )

                _:_ ->
                  ( "UNUSED TYPE VARIABLES"
                  , Nothing
                  , ["Type","variables"]
                    ++ H.commaSep "and" id (map H.nameToDoc allUnusedNames)
                    ++ ["are","unused","in","the",backQuote typeName,"definition."]
                  , H.commaSep "and" H.dullyellow (map H.nameToDoc allUnusedNames)
                  )
          in
          Report.Report title aliasRegion [] $
            Report.toCodeSnippet source aliasRegion subRegion
              (
                H.fillSep overview
              ,
                H.stack
                  [ H.fillSep $
                      ["I","recommend","removing"] ++ stuff ++ ["from","the","declaration,","like","this:"]
                  , H.indent 4 $ H.hsep $
                      ["type","alias",H.green (H.nameToDoc typeName)]
                      ++ map H.nameToDoc (filter (`notElem` allUnusedNames) allVars)
                      ++ ["=", "..."]
                  , H.reflow $
                      "Why? Well, if I allowed `type alias Height a = Float` I would need to answer\
                      \ some weird questions. Is `Height Bool` the same as `Float`? Is `Height Bool`\
                      \ the same as `Height Int`? My solution is to not need to ask them!"
                  ]
              )

        ([], unbound:unbounds) ->
          unboundTypeVars source aliasRegion ["type","alias"] typeName allVars unbound unbounds

        (_, _) ->
          let
            unused = map fst unusedVars
            unbound = map fst unboundVars

            theseAreUsed =
              case unbound of
                [x] ->
                  ["Type","variable",H.dullyellow ("`" <> H.nameToDoc x <> "`"),"appears"
                  ,"in","the","definition,","but","I","do","not","see","it","declared."
                  ]

                _ ->
                  ["Type","variables"]
                  ++ H.commaSep "and" H.dullyellow (map H.nameToDoc unbound)
                  ++ ["are","used","in","the","definition,","but","I","do","not","see","them","declared."]

            butTheseAreUnused =
              case unused of
                [x] ->
                  ["Likewise,","type","variable"
                  ,H.dullyellow ("`" <> H.nameToDoc x <> "`")
                  ,"is","delared,","but","not","used."
                  ]

                _ ->
                  ["Likewise,","type","variables"]
                  ++ H.commaSep "and" H.dullyellow (map H.nameToDoc unused)
                  ++ ["are","delared,","but","not","used."]

          in
          Report.Report "TYPE VARIABLE PROBLEMS" aliasRegion [] $
            Report.toCodeSnippet source aliasRegion Nothing
              (
                H.reflow $
                  "Type alias `" <> N.toString typeName <> "` has some type variable problems."
              ,
                H.stack
                  [ H.fillSep $ theseAreUsed ++ butTheseAreUnused
                  , H.reflow $
                      "My guess is that a definition like this will work better:"
                  , H.indent 4 $ H.hsep $
                      ["type", "alias", H.nameToDoc typeName]
                      ++ map H.nameToDoc (filter (`notElem` unused) allVars)
                      ++ map (H.green . H.nameToDoc) unbound
                      ++ ["=", "..."]
                  ]
              )



-- BAD TYPE VARIABLES


unboundTypeVars :: Code.Source -> R.Region -> [H.Doc] -> N.Name -> [N.Name] -> (N.Name, R.Region) -> [(N.Name, R.Region)] -> Report.Report
unboundTypeVars source declRegion tipe typeName allVars (unboundVar, varRegion) unboundVars =
  let
    backQuote name =
      "`" <> H.nameToDoc name <> "`"

    (title, subRegion, overview) =
      case map fst unboundVars of
        [] ->
          ( "UNBOUND TYPE VARIABLE"
          , Just varRegion
          , ["The",backQuote typeName]
            ++ tipe
            ++ ["uses","an","unbound","type","variable",H.dullyellow (backQuote unboundVar),"in","its","definition:"]
          )

        vars ->
          ( "UNBOUND TYPE VARIABLES"
          , Nothing
          , ["Type","variables"]
            ++ H.commaSep "and" H.dullyellow (H.nameToDoc unboundVar : map H.nameToDoc vars)
            ++ ["are","unbound","in","the",backQuote typeName] ++ tipe ++ ["definition:"]
          )
  in
  Report.Report title declRegion [] $
    Report.toCodeSnippet source declRegion subRegion
      (
        H.fillSep overview
      ,
        H.stack
          [ H.reflow $
              "You probably need to change the declaration to something like this:"
          , H.indent 4 $ H.hsep $
              tipe
              ++ [H.nameToDoc typeName]
              ++ map H.nameToDoc allVars
              ++ map (H.green . H.nameToDoc) (unboundVar : map fst unboundVars)
              ++ ["=", "..."]
          , H.reflow $
              "Why? Well, imagine one `" ++ N.toString typeName ++ "` where `" ++ N.toString unboundVar ++
              "` is an Int and another where it is a Bool. When we explicitly list the type\
              \ variables, the type checker can see that they are actually different types."
          ]
      )



-- NAME CLASH


nameClash :: Code.Source -> R.Region -> R.Region -> String -> Report.Report
nameClash source r1 r2 messageThatEndsWithPunctuation =
  Report.Report "NAME CLASH" r2 [] $
    Report.toCodePair source r1 r2
      (
        H.reflow messageThatEndsWithPunctuation
      ,
        "How can I know which one you want? Rename one of them!"
      )
      (
        H.reflow (messageThatEndsWithPunctuation <> " One here:")
      ,
        "And another one here:"
      ,
        "How can I know which one you want? Rename one of them!"
      )



-- AMBIGUOUS NAME


ambiguousName :: Code.Source -> R.Region -> Maybe N.Name -> N.Name -> [ModuleName.Canonical] -> String -> Report.Report
ambiguousName source region maybePrefix name possibleHomes thing =
  Report.Report "AMBIGUOUS NAME" region [] $
    Report.toCodeSnippet source region Nothing $
      case maybePrefix of
        Nothing ->
          let
            homeToYellowDoc (ModuleName.Canonical _ home) =
              H.dullyellow (H.nameToDoc home)

            bothOrAll =
              if length possibleHomes == 2 then "both" else "all"
          in
          (
            H.reflow $ "This usage of `" ++ N.toString name ++ "` is ambiguous."
          ,
            H.stack
              [ H.reflow $
                  "Check your imports. The following modules " ++ bothOrAll
                  ++ " expose a `" ++ N.toString name ++ "` " ++ thing ++ ":"
              , H.indent 4 $ H.vcat $ map homeToYellowDoc possibleHomes
              , H.reflowLink "Read" "imports" "to learn how to clarify which one you want."
              ]
          )

        Just prefix ->
          let
            homeToYellowDoc (ModuleName.Canonical _ home) =
              if prefix == home then
                H.blue "import" <+> H.dullyellow (H.nameToDoc home)
              else
                H.blue "import" <+> H.dullyellow (H.nameToDoc home) <+> H.blue "as" <+> H.dullyellow (H.nameToDoc prefix)

            eitherOrAny =
              if length possibleHomes == 2 then "either" else "any"
          in
          (
            H.reflow $ "This usage of `" ++ toQualString prefix name ++ "` is ambiguous."
          ,
            H.stack
              [ H.reflow $
                  "It could refer to a " ++ thing ++ " from "
                  ++ eitherOrAny ++ " of these imports:"
              , H.indent 4 $ H.vcat $ map homeToYellowDoc possibleHomes
              , H.reflowLink "Read" "imports" "to learn how to clarify which one you want."
              ]
          )



-- NOT FOUND


notFound :: Code.Source -> R.Region -> Maybe N.Name -> N.Name -> String -> PossibleNames -> Report.Report
notFound source region maybePrefix name thing (PossibleNames locals quals) =
  let
    givenName =
      maybe N.toString toQualString maybePrefix name

    possibleNames =
      let
        addQuals prefix localSet allNames =
          Set.foldr (\x xs -> toQualString prefix x : xs) allNames localSet
      in
      Map.foldrWithKey addQuals (map N.toString (Set.toList locals)) quals

    nearbyNames =
      take 4 (H.nearbyNames id givenName possibleNames)

    toDetails noSuggestionDetails yesSuggestionDetails =
      case nearbyNames of
        [] ->
          H.stack
            [ H.reflow noSuggestionDetails
            , H.link "Hint" "Read" "imports" "to see how `import` declarations work in Elm."
            ]

        suggestions ->
          H.stack
            [ H.reflow yesSuggestionDetails
            , H.indent 4 $ H.vcat $ map H.dullyellow $ map H.text suggestions
            , H.link "Hint" "Read" "imports" "to see how `import` declarations work in Elm."
            ]

  in
  Report.Report "NAMING ERROR" region nearbyNames $
    Report.toCodeSnippet source region Nothing
      (
        H.reflow $
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
                  ("I cannot find a `" ++ N.toString prefix ++ "` module. Is there an `import` for it?")
                  ("I cannot find a `" ++ N.toString prefix ++ "` import. These names seem close though:")

              Just _ ->
                toDetails
                  ("The `" ++ N.toString prefix ++ "` module does not expose a `" ++ N.toString name ++ "` " ++ thing ++ ".")
                  ("The `" ++ N.toString prefix ++ "` module does not expose a `" ++ N.toString name ++ "` " ++ thing ++ ". These names seem close though:")
      )


toQualString :: N.Name -> N.Name -> String
toQualString prefix name =
  N.toString prefix ++ "." ++ N.toString name



{-- VAR ERROR


varErrorToReport :: VarError -> Report.Report
varErrorToReport (VarError kind name problem suggestions) =
  let
    learnMore orMaybe =
      H.reflow $
        orMaybe <> " `import` works different than you expect? Learn all about it here: "
        <> H.hintLink "imports"

    namingError overview maybeStarter specializedSuggestions =
      Report.reportDoc "NAMING ERROR" Nothing overview $
        case H.maybeYouWant' maybeStarter specializedSuggestions of
          Nothing ->
            learnMore "Maybe"
          Just doc ->
            H.stack [ doc, learnMore "Or maybe" ]

    specialNamingError specialHint =
      Report.reportDoc "NAMING ERROR" Nothing (cannotFind kind name) (H.hsep specialHint)
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
  [ "Cannot", "find", a, thing, "named", H.dullyellow name <> ":" ]


ambiguous :: VarKind -> Text -> [Doc]
ambiguous kind rawName =
  let ( _a, thing, name ) = toKindInfo kind rawName in
  [ "This", "usage", "of", "the", H.dullyellow name, thing, "is", "ambiguous." ]


notEqualsHint :: Text -> [Doc]
notEqualsHint op =
  [ "Looking", "for", "the", "“not", "equal”", "operator?", "The", "traditional"
  , H.dullyellow $ text $ "(" <> op <> ")"
  , "is", "replaced", "by", H.green "(/=)", "in", "Elm.", "It", "is", "meant"
  , "to", "look", "like", "the", "“not", "equal”", "sign", "from", "math!", "(≠)"
  ]


equalsHint :: [Doc]
equalsHint =
  [ "A", "special", H.dullyellow "(===)", "operator", "is", "not", "needed"
  , "in", "Elm.", "We", "use", H.green "(==)", "for", "everything!"
  ]


modHint :: [Doc]
modHint =
  [ "Rather", "than", "a", H.dullyellow "(%)", "operator,"
  , "Elm", "has", "a", H.green "modBy", "function."
  , "Learn", "more", "here:"
  , "<https://package.elm-lang.org/packages/elm-lang/core/latest/Basics#modBy>"
  ]


-}


-- ARG MISMATCH


_argMismatchReport :: Code.Source -> R.Region -> String -> N.Name -> Int -> Int -> Report.Report
_argMismatchReport source region kind name expected actual =
  let
    numArgs =
      "too "
      <> (if actual < expected then "few" else "many")
      <> " arguments"
  in
    Report.Report (map Char.toUpper numArgs) region [] $
      Report.toCodeSnippet source region Nothing
        (
          H.reflow $
            kind <> " " <> N.toString name <> " has " <> numArgs <> "."
        ,
          H.reflow $
            "Expecting " <> show expected <> ", but got " <> show actual <> "."
        )



-- BAD ALIAS RECURSION


aliasRecursionReport :: Code.Source -> R.Region -> N.Name -> [N.Name] -> Src.Type -> [N.Name] -> Report.Report
aliasRecursionReport source region name args tipe others =
  case others of
    [] ->
      Report.Report "ALIAS PROBLEM" region [] $
        Report.toCodeSnippet source region Nothing
          (
            "This type alias is recursive, forming an infinite type!"
          ,
            H.stack
              [ H.reflow $
                  "When I expand a recursive type alias, it just keeps getting bigger and bigger.\
                  \ So dealiasing results in an infinitely large type! Try this instead:"
              , H.indent 4 $
                  aliasToUnionDoc name args tipe
              , H.link "Hint"
                  "This is kind of a subtle distinction. I suggested the naive fix, but I recommend reading"
                  "recursive-alias"
                  "for ideas on how to do better."
              ]
          )

    _ ->
      Report.Report "ALIAS PROBLEM" region [] $
        Report.toCodeSnippet source region Nothing
          (
            "This type alias is part of a mutually recursive set of type aliases."
          ,
            H.stack
              [ "It is part of this cycle of type aliases:"
              , H.indent 4 (H.drawCycle (name:others))
              , H.reflow $
                  "You need to convert at least one of these type aliases into a `type`."
              , H.link "Note" "Read" "recursive-alias"
                  "to learn why this `type` vs `type alias` distinction matters. It is subtle but important!"
              ]
          )


aliasToUnionDoc :: N.Name -> [N.Name] -> Src.Type -> Doc
aliasToUnionDoc name args tipe =
  H.vcat
    [ H.dullyellow $
        "type" <+> H.nameToDoc name <+> (foldr (<+>) "=" (map H.nameToDoc args))
    , H.green $
        H.indent 4 (H.nameToDoc name)
    , H.dullyellow $
        H.indent 8 (RT.srcToDoc RT.App tipe)
    ]
