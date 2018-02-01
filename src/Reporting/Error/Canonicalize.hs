{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Reporting.Error.Canonicalize
  ( Error(..)
  , BadArityContext(..)
  , InvalidPayload(..)
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
import qualified Reporting.Render.Type as RenderType
import qualified Reporting.Report as Report
import qualified Reporting.Helpers as H
import Reporting.Helpers ( Doc, (<>) )



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
  | ImportNotFound R.Region N.Name [N.Name]
  | ImportOpenAlias R.Region N.Name
  | ImportExposingNotFound R.Region N.Name N.Name [N.Name]
  | NotFoundVar R.Region N.Name PossibleNames
  | NotFoundType R.Region N.Name PossibleNames
  | NotFoundCtor R.Region N.Name PossibleNames
  | NotFoundBinop R.Region N.Name (Set.Set N.Name)
  | NotFoundVarQual R.Region N.Name N.Name PossibleNames
  | NotFoundTypeQual R.Region N.Name N.Name PossibleNames
  | NotFoundCtorQual R.Region N.Name N.Name PossibleNames
  | PatternHasRecordCtor R.Region N.Name
  | PortPayloadInvalid R.Region N.Name Can.Type InvalidPayload
  | PortTypeInvalid R.Region N.Name Can.Type
  | RecursiveAlias R.Region N.Name [N.Name] Src.Type [N.Name]
  | RecursiveDecl [Can.Def]
  | RecursiveLet (A.Located N.Name) [N.Name]
  | Shadowing N.Name R.Region R.Region
  | TupleLargerThanThree R.Region
  | TypeVarsUnboundInUnion R.Region N.Name [N.Name] [A.Located N.Name]
  | TypeVarsMessedUpInAlias R.Region N.Name [N.Name] [A.Located N.Name] [A.Located N.Name]


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


toReport :: Code.Source -> RenderType.Localizer -> Error -> Report.Report
toReport source localizer err =
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
      error "TODO" region maybePrefix name possibleHomes

    AmbiguousType region maybePrefix name possibleHomes ->
      error "TODO" region maybePrefix name possibleHomes

    AmbiguousCtor region maybePrefix name possibleHomes ->
      error "TODO" region maybePrefix name possibleHomes

    AmbiguousBinop region name possibleHomes ->
      error "TODO" region name possibleHomes

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

    ExportNotFound region kind rawName suggestions ->
      Report.Report "UNKNOWN EXPORT" region suggestions $
        let (a, thing, name) = toKindInfo kind rawName in
        H.stack
          [ H.fillSep
              ["You","are","trying","to","expose",a,thing,"named"
              ,name,"but","I","cannot","find","its","definition."
              ]
          , H.maybeYouWant Nothing suggestions
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

    ImportNotFound region name suggestions ->
      Report.Report "UNKNOWN IMPORT" region suggestions $
        Report.toCodeSnippet source region Nothing
          (
            H.reflow $
              "I could not find a `" <> N.toString name <> "` module to import!"
          ,
            H.maybeYouWant Nothing suggestions
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

    ImportExposingNotFound region name value suggestions ->
      Report.Report "BAD IMPORT" region suggestions $
        Report.toCodeSnippet source region Nothing
          (
            H.reflow $
              "The `" <> N.toString name
              <> "` module does not expose `"
              <> N.toString value <> "`:"
          ,
            H.maybeYouWant Nothing suggestions
          )

    NotFoundVar region name possibleNames ->
      error "TODO" region name possibleNames

    NotFoundType region name possibleNames ->
      error "TODO" region name possibleNames

    NotFoundCtor region name possibleNames ->
      error "TODO" region name possibleNames

    NotFoundBinop region name locals ->
      error "TODO" region name locals

    NotFoundVarQual region prefix name possibleNames ->
      error "TODO" region prefix name possibleNames

    NotFoundTypeQual region prefix name possibleNames ->
      error "TODO" region prefix name possibleNames

    NotFoundCtorQual region prefix name possibleNames ->
      error "TODO" region prefix name possibleNames

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

    PortPayloadInvalid region portName badType invalidPayload ->
      let
        (aBadKindOfThing, elaboration) =
          case invalidPayload of
            ExtendedRecord ->
              (
                "an extended record"
              ,
                H.reflow $
                  "The exact shape of the record must be known at compile time. No type variables!"
              )

            Function ->
              (
                "a function"
              ,
                H.reflow $
                  "Functions cannot be sent in and out ports. If we allowed functions in from JS\
                  \ they may perform some side-effects. If we let functions out, they could produce\
                  \ incorrect results because Elm optimizations assume there are no side-effects."
              )


            TypeVariable name ->
              (
                "an unspecified type"
              ,
                H.reflow $
                  "Type variables like `" <> N.toString name <> "` cannot flow through ports because\
                  \ the marshalling code needs to know the exact data that it is going to process."
              )

            UnsupportedType name ->
              (
                "a `" <> N.toString name <> "` value"
              ,
                H.stack
                  [ H.reflow $ "The types that can flow in and out of Elm include:"
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
      in
        Report.Report "PORT ERROR" region [] $
          Report.toCodeSnippet source region Nothing
            (
              H.reflow $
                "The `" <> N.toString portName <> "` port is trying to transmit " <> aBadKindOfThing <> ":"
            ,
              H.stack
                [ error "TODO" badType
                , elaboration
                , H.link "Hint"
                    "Ports are not a traditional FFI for calling JS functions directly. They need a different mindset! Read"
                    "ports"
                    "to learn how to use ports effectively."
                ]
            )

    PortTypeInvalid region name badType ->
      Report.Report "BAD PORT" region [] $
        Report.toCodeSnippet source region Nothing
          (
            H.reflow $
              "Port `" <> N.toString name <> "` has an invalid type."
          ,
            error "TODO PortTypeInvalid" badType
          )

    RecursiveAlias region name args tipe others ->
        aliasRecursionReport source localizer region name args tipe others

    RecursiveDecl cyclicValueDefs ->
        error "TODO" cyclicValueDefs

    RecursiveLet (A.At region name) names ->
      Report.Report "CYCLIC VALUE" region [] $
        Report.toCodeSnippet source region Nothing $
          case names of
            [] ->
                (
                  H.reflow $
                    "The `" <> N.toString name <> "` value has a direct use of `" <> N.toString name <> "` in its definition."
                ,
                  -- TODO see if shadowing rules out mutation attempts
                  H.stack
                    [ H.reflow $
                        "To know what `" <> N.toString name
                        <> "` is, I need to know what `" <> N.toString name
                        <> "` is. Let me recursively figure that out. Hmm, I need to know what `"
                        <> N.toString name <> "` is..."
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

    TypeVarsUnboundInUnion _ _ _ _ ->
      error "TODO TypeVarsUnboundInUnion"

    TypeVarsMessedUpInAlias _ _ _ _ _ ->
      error "TODO TypeVarsMessedUpInAlias"




--    UnboundTypeVarsInUnion typeName givenVars unbound ->
--        unboundTypeVars "type" typeName givenVars unbound

--    UnboundTypeVarsInAlias typeName givenVars unbound ->
--        unboundTypeVars "type alias" typeName givenVars unbound

--    UnusedTypeVarsInAlias typeName givenVars unused ->
--        Report.report
--          "UNUSED TYPE VARIABLES"
--          Nothing
--          ( "Type alias `" <> typeName <> "` cannot have unused type variables: "
--            <> H.commaSep unused
--          )
--          ( H.stack
--              [ text "You probably need to change the declaration like this:"
--              , H.dullyellow $ hsep $
--                  map text ("type" : "alias" : typeName : filter (`notElem` unused) givenVars ++ ["=", "..."])
--              ]
--          )

--    MessyTypeVarsInAlias typeName givenVars unused unbound ->
--        Report.report
--          "TYPE VARIABLE PROBLEMS"
--          Nothing
--          ( "Type alias `" <> typeName <> "` has some problems with type variables."
--          )
--          ( H.stack
--              [ H.reflow $
--                  "The declaration says it uses certain type variables ("
--                  <> H.commaSep unused <> ") but they do not appear in the aliased type. "
--                  <> "Furthermore, the aliased type says it uses type variables ("
--                  <> H.commaSep unbound
--                  <> ") that do not appear in the declaration."
--              , text "You probably need to change the declaration like this:"
--              , H.dullyellow $ hsep $
--                  map text ("type" : "alias" : typeName : filter (`notElem` unused) givenVars ++ unbound ++ ["=", "..."])
--              ]
--          )


--unboundTypeVars :: Text -> Text -> [Text] -> [Text] -> Report.Report
--unboundTypeVars declKind typeName givenVars unboundVars =
--  Report.report
--    "UNBOUND TYPE VARIABLES"
--    Nothing
--    ( H.capitalize declKind <> " `" <> typeName <> "` must declare its use of type variable"
--      <> H.commaSep unboundVars
--    )
--    ( H.stack
--        [ text "You probably need to change the declaration like this:"
--        , hsep $
--            map text (declKind : typeName : givenVars)
--            ++ map (H.dullyellow . text) unboundVars
--            ++ map text ["=", "..."]
--        , reflow $
--            "Here's why. Imagine one `" <> typeName <> "` where `" <> head unboundVars <>
--            "` is an Int and another where it is a Bool. When we explicitly list the type\
--            \ variables, the type checker can see that they are actually different types."
--        ]
--    )



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


aliasRecursionReport :: Code.Source -> RenderType.Localizer -> R.Region -> N.Name -> [N.Name] -> Src.Type -> [N.Name] -> Report.Report
aliasRecursionReport source localizer region name args tipe others =
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
                  RenderType.decl localizer name args [(name, [error "TODO" tipe])]
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
