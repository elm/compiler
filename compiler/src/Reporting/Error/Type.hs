{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Reporting.Error.Type
  ( Error(..)
  -- expectations
  , Expected(..)
  , Context(..)
  , SubContext(..)
  , MaybeName(..)
  , Category(..)
  , PExpected(..)
  , PContext(..)
  , PCategory(..)
  , typeReplace
  , ptypeReplace
  -- make reports
  , toReport
  )
  where


import Prelude hiding (round)
import qualified Data.Map as Map
import Data.Monoid ((<>))

import qualified AST.Canonical as Can
import qualified Data.Index as Index
import qualified Elm.Name as N
import qualified Reporting.Doc as D
import qualified Reporting.Region as R
import qualified Reporting.Render.Code as Code
import qualified Reporting.Render.Type as RT
import qualified Reporting.Render.Type.Localizer as L
import qualified Reporting.Report as Report
import qualified Reporting.Suggest as Suggest
import qualified Type.Error as T



-- ERRORS


data Error
  = BadExpr R.Region Category T.Type (Expected T.Type)
  | BadPattern R.Region PCategory T.Type (PExpected T.Type)
  | InfiniteType R.Region N.Name T.Type



-- EXPRESSION EXPECTATIONS


data Expected tipe
  = NoExpectation tipe
  | FromContext R.Region Context tipe
  | FromAnnotation N.Name Int SubContext tipe


data Context
  = ListEntry Index.ZeroBased
  | Negate
  | OpLeft N.Name
  | OpRight N.Name
  | IfCondition
  | IfBranch Index.ZeroBased
  | CaseBranch Index.ZeroBased
  | CallArity MaybeName Int
  | CallArg MaybeName Index.ZeroBased
  | RecordAccess R.Region (Maybe N.Name) R.Region N.Name
  | RecordUpdateKeys N.Name (Map.Map N.Name Can.FieldUpdate)
  | RecordUpdateValue N.Name
  | Destructure


data SubContext
  = TypedIfBranch Index.ZeroBased
  | TypedCaseBranch Index.ZeroBased
  | TypedBody


data MaybeName
  = FuncName N.Name
  | CtorName N.Name
  | OpName N.Name
  | NoName


data Category
  = List
  | Number
  | Float
  | String
  | Char
  | If
  | Case
  | CallResult MaybeName
  | Lambda
  | Accessor N.Name
  | Access N.Name
  | Record
  | Tuple
  | Unit
  | Shader
  | Effects
  | Local N.Name
  | Foreign N.Name



-- PATTERN EXPECTATIONS


data PExpected tipe
  = PNoExpectation tipe
  | PFromContext R.Region PContext tipe


data PContext
  = PTypedArg N.Name Index.ZeroBased
  | PCaseMatch Index.ZeroBased
  | PCtorArg N.Name Index.ZeroBased
  | PListEntry Index.ZeroBased
  | PTail


data PCategory
  = PRecord
  | PUnit
  | PTuple
  | PList
  | PCtor N.Name
  | PInt
  | PStr
  | PChr
  | PBool



-- HELPERS


typeReplace :: Expected a -> b -> Expected b
typeReplace expectation tipe =
  case expectation of
    NoExpectation _ ->
      NoExpectation tipe

    FromContext region context _ ->
      FromContext region context tipe

    FromAnnotation name arity context _ ->
      FromAnnotation name arity context tipe


ptypeReplace :: PExpected a -> b -> PExpected b
ptypeReplace expectation tipe =
  case expectation of
    PNoExpectation _ ->
      PNoExpectation tipe

    PFromContext region context _ ->
      PFromContext region context tipe



-- TO REPORT


toReport :: Code.Source -> L.Localizer -> Error -> Report.Report
toReport source localizer err =
  case err of
    BadExpr region category actualType expected ->
      toExprReport source localizer region category actualType expected

    BadPattern region category tipe expected ->
      toPatternReport source localizer region category tipe expected

    InfiniteType region name overallType ->
      toInfiniteReport source localizer region name overallType



-- TO PATTERN REPORT


toPatternReport :: Code.Source -> L.Localizer -> R.Region -> PCategory -> T.Type -> PExpected T.Type -> Report.Report
toPatternReport source localizer patternRegion category tipe expected =
  Report.Report "TYPE MISMATCH" patternRegion [] $
  case expected of
    PNoExpectation expectedType ->
      Report.toCodeSnippet source patternRegion Nothing $
        ( "This pattern is being used in an unexpected way:"
        , patternTypeComparison localizer tipe expectedType
            (addPatternCategory "It is" category)
            "But it needs to match:"
            []
        )

    PFromContext region context expectedType ->
      Report.toCodeSnippet source region (Just patternRegion) $
        case context of
          PTypedArg name index ->
            ( D.reflow $
                "The " <> D.ordinal index <> " argument to `" <> N.toString name <> "` is weird."
            , patternTypeComparison localizer tipe expectedType
                (addPatternCategory "The argument is a pattern that matches" category)
                ( "But the type annotation on `" <> N.toString name
                  <> "` says the " <> D.ordinal index <> " argument should be:"
                )
                []
            )

          PCaseMatch index ->
            if index == Index.first then
              (
                D.reflow $
                  "The 1st pattern in this `case` causing a mismatch:"
              ,
                patternTypeComparison localizer tipe expectedType
                  (addPatternCategory "The first pattern is trying to match" category)
                  "But the expression between `case` and `of` is:"
                  [ D.reflow $
                      "These can never match! Is the pattern the problem? Or is it the expression?"
                  ]
              )
            else
              ( D.reflow $
                  "The " <> D.ordinal index <> " pattern in this `case` does not match the previous ones."
              , patternTypeComparison localizer tipe expectedType
                  (addPatternCategory ("The " <> D.ordinal index <> " pattern is trying to match") category)
                  "But all the previous patterns match:"
                  [ D.link "Note"
                      "A `case` expression can only handle one type of value, so you may want to use"
                      "union-types"
                      "to handle “mixing” types."
                  ]
              )

          PCtorArg name index ->
            ( D.reflow $
                "The " <> D.ordinal index <> " argument to `" <> N.toString name <> "` is weird."
            , patternTypeComparison localizer tipe expectedType
                (addPatternCategory "It is trying to match" category)
                ( "But `" <> N.toString name <> "` needs its "
                  <> D.ordinal index <> " argument to be:"
                )
                []
            )

          PListEntry index ->
            ( D.reflow $
                "The " <> D.ordinal index <> " pattern in this list does not match all the previous ones:"
            , patternTypeComparison localizer tipe expectedType
                (addPatternCategory ("The " <> D.ordinal index <> " pattern is trying to match") category)
                "But all the previous patterns in the list are:"
                [ D.toSimpleHint $
                    "Everything in the list needs to be the same type of value.\
                    \ This way you never run into unexpected values partway through.\
                    \ To mix different types in a single list, create a \"union type\" as\
                    \ described in: <http://guide.elm-lang.org/types/union_types.html>"
                ]
            )

          PTail ->
            ( D.reflow $
                "The pattern after (::) is causing issues."
            , patternTypeComparison localizer tipe expectedType
                (addPatternCategory "The pattern after (::) is trying to match" category)
                "But it needs to match lists like this:"
                []
            )



-- PATTERN HELPERS


patternTypeComparison :: L.Localizer -> T.Type -> T.Type -> String -> String -> [D.Doc] -> D.Doc
patternTypeComparison localizer actual expected iAmSeeing insteadOf contextHints =
  let
    (actualDoc, expectedDoc, problems) =
      T.toComparison localizer actual expected
  in
  D.stack $
    [ D.reflow iAmSeeing
    , D.indent 4 actualDoc
    , D.reflow insteadOf
    , D.indent 4 expectedDoc
    ]
    ++ problemsToHint problems
    ++ contextHints


addPatternCategory :: String -> PCategory -> String
addPatternCategory iAmTryingToMatch category =
  iAmTryingToMatch <>
    case category of
      PRecord -> " record values of type:"
      PUnit -> " unit values:"
      PTuple -> " tuples of type:"
      PList -> " lists of type:"
      PCtor name -> " `" <> N.toString name <> "` values of type:"
      PInt -> " integers:"
      PStr -> " strings:"
      PChr -> " characters:"
      PBool -> " booleans:"



-- EXPR HELPERS


typeComparison :: L.Localizer -> T.Type -> T.Type -> String -> String -> [D.Doc] -> D.Doc
typeComparison localizer actual expected iAmSeeing insteadOf contextHints =
  let
    (actualDoc, expectedDoc, problems) =
      T.toComparison localizer actual expected
  in
  D.stack $
    [ D.reflow iAmSeeing
    , D.indent 4 actualDoc
    , D.reflow insteadOf
    , D.indent 4 expectedDoc
    ]
    ++ contextHints
    ++ problemsToHint problems


loneType :: L.Localizer -> T.Type -> T.Type -> D.Doc -> [D.Doc] -> D.Doc
loneType localizer actual expected iAmSeeing furtherDetails =
  let
    (actualDoc, _, problems) =
      T.toComparison localizer actual expected
  in
  D.stack $
    [ iAmSeeing
    , D.indent 4 actualDoc
    ]
    ++ furtherDetails
    ++ problemsToHint problems


addCategory :: String -> Category -> String
addCategory thisIs category =
  case category of
    Local name -> "This `" <> N.toString name <> "` value is a:"
    Foreign name -> "This `" <> N.toString name <> "` value is a:"
    Access field -> "The value at ." <> N.toString field <> " is a:"
    Accessor field -> "This ." <> N.toString field <> " field access function has type:"
    If -> "This `if` expression produces:"
    Case -> "This `case` expression produces:"
    List -> thisIs <> " a list of type:"
    Number -> thisIs <> " a number of type:"
    Float -> thisIs <> " a float of type:"
    String -> thisIs <> " a string of type:"
    Char -> thisIs <> " a character of type:"
    Lambda -> thisIs <> " an anonymous function of type:"
    Record -> thisIs <> " a record of type:"
    Tuple -> thisIs <> " a tuple of type:"
    Unit -> thisIs <> " a unit value:"
    Shader -> thisIs <> " a GLSL shader of type:"
    Effects -> thisIs <> " a thing for CORE LIBRARIES ONLY."
    CallResult maybeName ->
      case maybeName of
        NoName -> thisIs <> ":"
        FuncName name -> "This `" <> N.toString name <> "` call produces:"
        CtorName name -> "This `" <> N.toString name <> "` call produces:"
        OpName _ -> thisIs <> ":"


problemsToHint :: [T.Problem] -> [D.Doc]
problemsToHint problems =
  case problems of
    [] ->
      []

    problem : _ ->
      problemToHint problem


problemToHint :: T.Problem -> [D.Doc]
problemToHint problem =
  case problem of
    T.IntFloat ->
      [ D.fancyLink "Note" ["Read"] "implicit-casts"
          ["to","learn","why","Elm","does","not","implicitly","convert"
          ,"Ints","to","Floats.","Use",D.green "toFloat","and"
          ,D.green "round","to","do","explicit","conversions."
          ]
      ]

    T.StringFromInt ->
      [ D.toFancyHint
          ["Want","to","convert","an","Int","into","a","String?"
          ,"Use","the",D.green "String.fromInt","function!"
          ]
      ]

    T.StringFromFloat ->
      [ D.toFancyHint
          ["Want","to","convert","a","Float","into","a","String?"
          ,"Use","the",D.green "String.fromFloat","function!"
          ]
      ]

    T.StringToInt ->
      [ D.toFancyHint
          ["Want","to","convert","a","String","into","an","Int?"
          ,"Use","the",D.green "String.toInt","function!"
          ]
      ]

    T.StringToFloat ->
      [ D.toFancyHint
          ["Want","to","convert","a","String","into","a","Float?"
          ,"Use","the",D.green "String.toFloat","function!"
          ]
      ]

    T.AnythingToBool ->
      [ D.toSimpleHint $
          "Elm does not have “truthiness” such that ints and strings and lists\
          \ are automatically converted to booleans. Do that conversion explicitly!"
      ]

    T.AnythingFromMaybe ->
      [ D.toFancyHint
          ["Use",D.green "Maybe.withDefault","to","handle","possible","errors."
          ,"Longer","term,","it","is","usually","better","to","write","out","the"
          ,"full","`case`","though!"
          ]
      ]

    T.AnythingToList ->
      [ D.toSimpleHint "Did you forget to add [] around it?"
      ]

    T.MissingArgs _ -> []
    T.ReturnMismatch -> []

    T.BadFlexSuper super _ tipe ->
      case tipe of
        T.Lambda _ _ _   -> badFlexSuper super tipe
        T.Infinite       -> []
        T.Error          -> []
        T.FlexVar _      -> []
        T.FlexSuper s _  -> badFlexFlexSuper super s
        T.RigidVar y     -> badRigidVar y (toASuperThing super)
        T.RigidSuper s _ -> badRigidSuper s (toASuperThing super)
        T.Type _ _ _     -> badFlexSuper super tipe
        T.Record _ _     -> badFlexSuper super tipe
        T.Unit           -> badFlexSuper super tipe
        T.Tuple _ _ _    -> badFlexSuper super tipe
        T.Alias _ _ _ _  -> badFlexSuper super tipe

    T.BadRigidVar x tipe ->
      case tipe of
        T.Lambda _ _ _   -> badRigidVar x "a function"
        T.Infinite       -> []
        T.Error          -> []
        T.FlexVar _      -> []
        T.FlexSuper s _  -> badRigidVar x (toASuperThing s)
        T.RigidVar y     -> badDoubleRigid x y
        T.RigidSuper _ y -> badDoubleRigid x y
        T.Type _ n _     -> badRigidVar x ("a `" ++ N.toString n ++ "` value")
        T.Record _ _     -> badRigidVar x "a record"
        T.Unit           -> badRigidVar x "a unit value"
        T.Tuple _ _ _    -> badRigidVar x "a tuple"
        T.Alias _ n _ _  -> badRigidVar x ("a `" ++ N.toString n ++ "` value")

    T.BadRigidSuper super x tipe ->
      case tipe of
        T.Lambda _ _ _   -> badRigidSuper super "a function"
        T.Infinite       -> []
        T.Error          -> []
        T.FlexVar _      -> []
        T.FlexSuper s _  -> badRigidSuper super (toASuperThing s)
        T.RigidVar y     -> badDoubleRigid x y
        T.RigidSuper _ y -> badDoubleRigid x y
        T.Type _ n _     -> badRigidSuper super ("a `" ++ N.toString n ++ "` value")
        T.Record _ _     -> badRigidSuper super "a record"
        T.Unit           -> badRigidSuper super "a unit value"
        T.Tuple _ _ _    -> badRigidSuper super "a tuple"
        T.Alias _ n _ _  -> badRigidSuper super ("a `" ++ N.toString n ++ "` value")

    T.FieldsMissing fields ->
      case map (D.green . D.fromName) fields of
        [] ->
          []

        [f1] ->
          [ D.toFancyHint ["Looks","like","the",f1,"field","is","missing."]
          ]

        fieldDocs ->
          [ D.toFancyHint $
              ["Looks","like","fields"] ++ D.commaSep "and" id fieldDocs ++ ["are","missing."]
          ]


    T.FieldTypo typo possibilities ->
      case Suggest.sort (N.toString typo) N.toString possibilities of
        [] ->
          []

        nearest:_ ->
          [ D.toFancyHint $
              ["Seems","like","a","record","field","typo.","Maybe"
              ,D.dullyellow (D.fromName typo),"should","be"
              ,D.green (D.fromName nearest) <> "?"
              ]
          , D.toSimpleHint
              "Can more type annotations be added? Type annotations always help me give\
              \ more specific messages, and I think they could help a lot in this case!"
          ]



-- BAD RIGID HINTS


badRigidVar :: N.Name -> String -> [D.Doc]
badRigidVar name aThing =
  [ D.toSimpleHint $
      "Your type annotation uses type variable `" ++ N.toString name ++
      "` which means ANY type of value can flow through, but your code is saying it specifically wants "
      ++ aThing ++ ". Maybe change your type annotation to\
      \ be more specific? Maybe change the code to be more general?"
  , D.reflowLink "Read" "type-annotations" "for more advice!"
  ]


badDoubleRigid :: N.Name -> N.Name -> [D.Doc]
badDoubleRigid x y =
  [ D.toSimpleHint $
      "Your type annotation uses `" ++ N.toString x ++ "` and `" ++ N.toString y ++
      "` as separate type variables. Your code seems to be saying they are the\
      \ same though! Maybe they should be the same in your type annotation?\
      \ Maybe your code uses them in a weird way?"
  , D.reflowLink "Read" "type-annotations" "for more advice!"
  ]


toASuperThing :: T.Super -> String
toASuperThing super =
  case super of
    T.Number     -> "a `number` value"
    T.Comparable -> "a `comparable` value"
    T.CompAppend -> "a `compappend` value"
    T.Appendable -> "an `appendable` value"



-- BAD SUPER HINTS


badFlexSuper :: T.Super -> T.Type -> [D.Doc]
badFlexSuper super tipe =
  case super of
    T.Comparable ->
      [ D.toSimpleHint "Only ints, floats, chars, strings, lists, and tuples are comparable."
      ]

    T.Appendable ->
      case tipe of
        T.Type home name _ | T.isInt home name ->
          [ D.toFancyHint ["Try","using",D.green"String.fromInt","to","convert","it","to","a","string?"]
          ]

        T.Type home name _ | T.isFloat home name ->
          [ D.toFancyHint ["Try","using",D.green"String.fromFloat","to","convert","it","to","a","string?"]
          ]

        T.FlexSuper T.Number _ ->
          [ D.toFancyHint ["Try","using",D.green"String.fromInt","to","convert","it","to","a","string?"]
          ]

        _ ->
          [ D.toFancyHint ["Only","strings","and","lists","are","appendable.","Put","it","in",D.green "[]","to","make","it","a","list?"]
          ]

    T.CompAppend ->
      [ D.toSimpleHint "Only strings and lists are both comparable and appendable."
      ]

    T.Number ->
      case tipe of
        T.Type home name _ | T.isString home name ->
          [ D.toFancyHint ["Try","using",D.green"String.toInt","to","convert","it","to","a","number?"]
          ]

        _ ->
          [ D.toFancyHint ["Only",D.green "Int","and",D.green "Float","values","work","as","numbers."]
          ]


badRigidSuper :: T.Super -> String -> [D.Doc]
badRigidSuper super aThing =
  let
    (superType, manyThings) =
      case super of
        T.Number -> ("number", "ints AND floats")
        T.Comparable -> ("comparable", "ints, floats, chars, strings, lists, and tuples")
        T.Appendable -> ("appendable", "strings AND lists")
        T.CompAppend -> ("compappend", "strings AND lists")
  in
  [ D.toSimpleHint $
      "The `" ++ superType ++ "` in your type annotation is saying that "
      ++ manyThings ++ " can flow through, but your code is saying it specifically wants "
      ++ aThing ++ ". Maybe change your type annotation to\
      \ be more specific? Maybe change the code to be more general?"
  , D.reflowLink "Read" "type-annotations" "for more advice!"
  ]


badFlexFlexSuper :: T.Super -> T.Super -> [D.Doc]
badFlexFlexSuper s1 s2 =
  let
    likeThis super =
      case super of
        T.Number -> "a number"
        T.Comparable -> "comparable"
        T.CompAppend -> "a compappend"
        T.Appendable -> "appendable"
  in
    [ D.toSimpleHint $
        "There are no values in Elm that are both"
        ++ likeThis s1 ++ " and " ++ likeThis s2 ++ "."
    ]



-- TO EXPR REPORT


toExprReport :: Code.Source -> L.Localizer -> R.Region -> Category -> T.Type -> Expected T.Type -> Report.Report
toExprReport source localizer exprRegion category tipe expected =
  case expected of
    NoExpectation expectedType ->
      Report.Report "TYPE MISMATCH" exprRegion [] $
        Report.toCodeSnippet source exprRegion Nothing
          ( "This expression is being used in an unexpected way:"
          , typeComparison localizer tipe expectedType
              (addCategory "It is" category)
              "But you are trying to use it as:"
              []
          )

    FromAnnotation name _arity subContext expectedType ->
      let
        thing =
          case subContext of
            TypedIfBranch index   -> D.ordinal index <> " branch of this `if` expression:"
            TypedCaseBranch index -> D.ordinal index <> " branch of this `case` expression:"
            TypedBody             -> "body of the `" <> N.toString name <> "` definition:"

        itIs =
          case subContext of
            TypedIfBranch index   -> "The " <> D.ordinal index <> " branch is"
            TypedCaseBranch index -> "The " <> D.ordinal index <> " branch is"
            TypedBody             -> "The body is"
      in
      Report.Report "TYPE MISMATCH" exprRegion [] $
        Report.toCodeSnippet source exprRegion Nothing $
          ( D.reflow ("Something is off with the " <> thing)
          , typeComparison localizer tipe expectedType
              (addCategory itIs category)
              ("But the type annotation on `" <> N.toString name <> "` says it should be:")
              []
          )

    FromContext region context expectedType ->
      let
        mismatch (maybeHighlight, problem, thisIs, insteadOf, furtherDetails) =
          Report.Report "TYPE MISMATCH" exprRegion [] $
            Report.toCodeSnippet source region maybeHighlight
              ( D.reflow problem
              , typeComparison localizer tipe expectedType (addCategory thisIs category) insteadOf furtherDetails
              )

        badType (maybeHighlight, problem, thisIs, furtherDetails) =
          Report.Report "TYPE MISMATCH" exprRegion [] $
            Report.toCodeSnippet source region maybeHighlight
              ( D.reflow problem
              , loneType localizer tipe expectedType (D.reflow (addCategory thisIs category)) furtherDetails
              )

        custom maybeHighlight docPair =
          Report.Report "TYPE MISMATCH" exprRegion [] $
            Report.toCodeSnippet source region maybeHighlight docPair
      in
      case context of
        ListEntry index ->
          let ith = D.ordinal index in
          mismatch
          ( Just exprRegion
          , "The " <> ith <> " element of this list does not match all the previous elements:"
          , "The " <> ith <> " element is"
          , "But all the previous elements in the list are:"
          , [ D.toSimpleHint $
                "Everything in the list needs to be the same type of value.\
                \ This way you never run into unexpected values partway through.\
                \ To mix different types in a single list, create a \"union type\" as\
                \ described in: <http://guide.elm-lang.org/types/union_types.html>"
            ]
          )

        Negate ->
          badType
          ( Just exprRegion
          , "I do not know how to negate this type of value:"
          , "It is"
          , [ D.fillSep
                ["But","I","only","now","how","to","negate"
                ,D.dullyellow "Int","and",D.dullyellow "Float","values."
                ]
            ]
          )

        OpLeft op ->
          custom (Just exprRegion) $
            opLeftToDocs localizer category op tipe expectedType

        OpRight op ->
          case opRightToDocs localizer category op tipe expectedType of
            EmphBoth details ->
              custom Nothing details

            EmphRight details ->
              custom (Just exprRegion) details

        IfCondition ->
          badType
          ( Just exprRegion
          , "This `if` condition does not evaluate to a boolean value, True or False."
          , "It is"
          , [ D.fillSep ["But","I","need","this","`if`","condition","to","be","a",D.dullyellow "Bool","value."]
            ]
          )

        IfBranch index ->
          let ith = D.ordinal index in
          mismatch
          ( Just exprRegion
          , "The " <> ith <> " branch of this `if` does not match all the previous branches:"
          , "The " <> ith <> " branch is"
          , "But all the previous branches result in:"
          , [ D.link "Hint"
                "All branches in an `if` must produce the same type of values. This way, no\
                \ matter which branch we take, the result is always a consistent shape. Read"
                "union-types"
                "to learn how to “mix” types."
            ]
          )

        CaseBranch index ->
          let ith = D.ordinal index in
          mismatch
          ( Just exprRegion
          , "The " <> ith <> " branch of this `case` does not match all the previous branches:"
          , "The " <> ith <> " branch is"
          , "But all the previous branches result in:"
          , [ D.link "Hint"
                "All branches in a `case` must produce the same type of values. This way, no\
                \ matter which branch we take, the result is always a consistent shape. Read"
                "union-types"
                "to learn how to “mix” types."
            ]
          )

        CallArity maybeFuncName numGivenArgs ->
          Report.Report "TOO MANY ARGS" exprRegion [] $
          Report.toCodeSnippet source region (Just exprRegion) $
          case countArgs tipe of
            0 ->
              let
                thisValue =
                  case maybeFuncName of
                    NoName        -> "This value"
                    FuncName name -> "The `" <> N.toString name <> "` value"
                    CtorName name -> "The `" <> N.toString name <> "` value"
                    OpName op     -> "The (" <> N.toString op <> ") operator"
              in
              ( D.reflow $ thisValue <> " is not a function, but it was given " <> D.args numGivenArgs <> "."
              , D.reflow $ "Are there any missing commas? Or missing parentheses?"
              )

            n ->
              let
                thisFunction =
                  case maybeFuncName of
                    NoName        -> "This function"
                    FuncName name -> "The `" <> N.toString name <> "` function"
                    CtorName name -> "The `" <> N.toString name <> "` constructor"
                    OpName op     -> "The (" <> N.toString op <> ") operator"
              in
              ( D.reflow $ thisFunction <> " expects " <> D.args n <> ", but it got " <> show numGivenArgs <> " instead."
              , D.reflow $ "Are there any missing commas? Or missing parentheses?"
              )

        CallArg maybeFuncName index ->
          let
            ith = D.ordinal index

            thisFunction =
              case maybeFuncName of
                NoName        -> "this function"
                FuncName name -> "`" <> N.toString name <> "`"
                CtorName name -> "`" <> N.toString name <> "`"
                OpName op     -> "(" <> N.toString op <> ")"
          in
          mismatch
          ( Just exprRegion
          , "The " <> ith <> " argument to " <> thisFunction <> " is not what I expect:"
          , "This argument is"
          , "But " <> thisFunction <> " needs the " <> ith <> " argument to be:"
          ,
            if Index.toHuman index == 1 then
              []
            else
              [ D.toSimpleHint $
                 "I always figure out the argument types from left to right. If an argument\
                  \ is acceptable, I assume it is “correct” and move on. So the problem may\
                  \ actually be in one of the previous arguments!"
              ]
          )

        RecordAccess recordRegion maybeName fieldRegion field ->
          case T.iteratedDealias tipe of
            T.Record fields ext ->
              custom (Just fieldRegion)
                ( D.reflow $
                    "This "
                    <> maybe "" (\n -> "`" <> N.toString n <> "`") maybeName
                    <> " record does not have a `" <> N.toString field <> "` field:"
                , case Suggest.sort (N.toString field) (N.toString . fst) (Map.toList fields) of
                    [] ->
                      D.reflow "In fact, it is a record with NO fields!"

                    f:fs ->
                      D.stack
                        [ D.reflow $
                            "This is usually a typo. Here are the "
                            <> maybe "" (\n -> "`" <> N.toString n <> "`") maybeName
                            <> " fields that are most similar:"
                        , toNearbyRecord localizer f fs ext
                        , D.fillSep
                            ["So","maybe",D.dullyellow (D.fromName field)
                            ,"should","be",D.green (D.fromName (fst f)) <> "?"
                            ]
                        ]
                )

            _ ->
              badType
              ( Just recordRegion
              , "This is not a record, so it has no fields to access!"
              , "It is"
              , [ D.fillSep
                    ["But","I","need","a","record","with","a"
                    ,D.dullyellow (D.fromName field),"field!"
                    ]
                ]
              )

        RecordUpdateKeys record expectedFields ->
          case T.iteratedDealias tipe of
            T.Record actualFields ext ->
              case Map.lookupMin (Map.difference expectedFields actualFields) of
                Nothing ->
                  mismatch
                  ( Nothing
                  , "Something is off with this record update:"
                  , "The `" <> N.toString record <> "` record is"
                  , "But this update needs it to be compatable with:"
                  , [ D.reflow
                        "Do you mind creating an <http://sscce.org/> that produces this error message and\
                        \ sharing it at <https://github.com/elm/error-message-catalog/issues> so we\
                        \ can try to give better advice here?"
                    ]
                  )

                Just (field, Can.FieldUpdate fieldRegion _) ->
                  let
                    rStr = "`" <> N.toString record <> "`"
                    fStr = "`" <> N.toString field <> "`"
                  in
                  custom (Just fieldRegion)
                    ( D.reflow $
                        "The " <> rStr <> " record does not have a " <> fStr <> " field:"
                    , case Suggest.sort (N.toString field) (N.toString . fst) (Map.toList actualFields) of
                        [] ->
                          D.reflow $ "In fact, " <> rStr <> " is a record with NO fields!"

                        f:fs ->
                          D.stack
                            [ D.reflow $
                                "This is usually a typo. Here are the " <> rStr <> " fields that are most similar:"
                            , toNearbyRecord localizer f fs ext
                            , D.fillSep
                                ["So","maybe",D.dullyellow (D.fromName field)
                                ,"should","be",D.green (D.fromName (fst f)) <> "?"
                                ]
                            ]
                    )

            _ ->
              badType
              ( Just exprRegion
              , "This is not a record, so it has no fields to update!"
              , "It is"
              , [ D.reflow $ "But I need a record!"
                ]
              )

        RecordUpdateValue field ->
          mismatch
          ( Just exprRegion
          , "I cannot update the `" <> N.toString field <> "` field like this:"
          , "You are trying to update `" <> N.toString field <> "` to be"
          , "But it should be:"
          , [ D.toSimpleNote
                "The record update syntax does not allow you to change the type of fields.\
                \ You can achieve that with record constructors or the record literal syntax."
            ]
          )

        Destructure ->
          mismatch
          ( Nothing
          , "This definition is causing issues:"
          , "You are defining"
          , "But then trying to destructure it as:"
          , []
          )



-- HELPERS


countArgs :: T.Type -> Int
countArgs tipe =
  case tipe of
    T.Lambda _ _ stuff ->
      1 + length stuff

    _ ->
      0



-- FIELD NAME HELPERS


toNearbyRecord :: L.Localizer -> (N.Name, T.Type) -> [(N.Name, T.Type)] -> T.Extension -> D.Doc
toNearbyRecord localizer f fs ext =
  D.indent 4 $
    if length fs <= 3 then
      RT.vrecord (map (fieldToDocs localizer) (f:fs)) (extToDoc ext)
    else
      RT.vrecordSnippet (fieldToDocs localizer f) (map (fieldToDocs localizer) (take 3 fs))


fieldToDocs :: L.Localizer -> (N.Name, T.Type) -> (D.Doc, D.Doc)
fieldToDocs localizer (name, tipe) =
  ( D.fromName name
  , T.toDoc localizer RT.None tipe
  )


extToDoc :: T.Extension -> Maybe D.Doc
extToDoc ext =
  case ext of
    T.Closed      -> Nothing
    T.FlexOpen  x -> Just (D.fromName x)
    T.RigidOpen x -> Just (D.fromName x)



-- OP LEFT


opLeftToDocs :: L.Localizer -> Category -> N.Name -> T.Type -> T.Type -> (D.Doc, D.Doc)
opLeftToDocs localizer category op tipe expected =
  case op of
    "+"
      | isString tipe -> badStringAdd
      | isList tipe   -> badListAdd localizer category "left" tipe expected
      | otherwise     -> badMath localizer category "Addition" "left" "+" tipe expected []

    "*"
      | isList tipe  -> badListMul localizer category "left" tipe expected
      | otherwise    -> badMath localizer category "Multiplication" "left" "*" tipe expected []

    "-"  -> badMath localizer category "Subtraction" "left" "-" tipe expected []
    "^"  -> badMath localizer category "Exponentiation" "left" "^" tipe expected []
    "/"  -> badFDiv localizer "left" tipe expected
    "//" -> badIDiv localizer "left" tipe expected
    "&&" -> badBool localizer "&&" "left" tipe expected
    "||" -> badBool localizer "||" "left" tipe expected
    "<"  -> badCompLeft localizer category "<" "left" tipe expected
    ">"  -> badCompLeft localizer category ">" "left" tipe expected
    "<=" -> badCompLeft localizer category "<=" "left" tipe expected
    ">=" -> badCompLeft localizer category ">=" "left" tipe expected

    "++" -> badAppendLeft localizer category tipe expected

    "<|" ->
      ( "The left side of (<|) needs to be a function so I can pipe arguments to it!"
      , loneType localizer tipe expected
          (D.reflow (addCategory "I am seeing" category))
          [ D.reflow $ "This needs to be some kind of function though!"
          ]
      )

    _ ->
      ( D.reflow $
          "The left argument of (" <> N.toString op <> ") is causing problems:"
      , typeComparison localizer tipe expected
          (addCategory "The left argument is" category)
          ("But (" <> N.toString op <> ") needs the left argument to be:")
          []
      )



-- OP RIGHT


data RightDocs
  = EmphBoth (D.Doc, D.Doc)
  | EmphRight (D.Doc, D.Doc)


opRightToDocs :: L.Localizer -> Category -> N.Name -> T.Type -> T.Type -> RightDocs
opRightToDocs localizer category op tipe expected =
  case op of
    "+"
      | isFloat expected && isInt tipe -> badCast op FloatInt
      | isInt expected && isFloat tipe -> badCast op IntFloat
      | isString tipe -> EmphRight $ badStringAdd
      | isList tipe   -> EmphRight $ badListAdd localizer category "right" tipe expected
      | otherwise     -> EmphRight $ badMath localizer category "Addition" "right" "+" tipe expected []

    "*"
      | isFloat expected && isInt tipe -> badCast op FloatInt
      | isInt expected && isFloat tipe -> badCast op IntFloat
      | isList tipe -> EmphRight $ badListMul localizer category "right" tipe expected
      | otherwise   -> EmphRight $ badMath localizer category "Multiplication" "right" "*" tipe expected []

    "-"
      | isFloat expected && isInt tipe -> badCast op FloatInt
      | isInt expected && isFloat tipe -> badCast op IntFloat
      | otherwise ->
          EmphRight $ badMath localizer category "Subtraction" "right" "-" tipe expected []

    "^"
      | isFloat expected && isInt tipe -> badCast op FloatInt
      | isInt expected && isFloat tipe -> badCast op IntFloat
      | otherwise ->
          EmphRight $ badMath localizer category "Exponentiation" "right" "^" tipe expected []

    "/"  -> EmphRight $ badFDiv localizer "right" tipe expected
    "//" -> EmphRight $ badIDiv localizer "right" tipe expected
    "&&" -> EmphRight $ badBool localizer "&&" "right" tipe expected
    "||" -> EmphRight $ badBool localizer "||" "right" tipe expected
    "<"  -> badCompRight localizer "<" tipe expected
    ">"  -> badCompRight localizer ">" tipe expected
    "<=" -> badCompRight localizer "<=" tipe expected
    ">=" -> badCompRight localizer ">=" tipe expected
    "==" -> badEquality localizer "==" tipe expected
    "/=" -> badEquality localizer "/=" tipe expected

    "::" -> badConsRight localizer category tipe expected
    "++" -> badAppendRight localizer category tipe expected

    "<|" ->
      EmphRight
        ( D.reflow $ "I cannot send this through the (<|) pipe:"
        , typeComparison localizer tipe expected
            "The argument is:"
            "But (<|) is piping it a function that expects:"
            []
        )

    "|>" ->
      case (tipe, expected) of
        (T.Lambda expectedArgType _ _, T.Lambda argType _ _) ->
          EmphRight
            ( D.reflow $ "This function cannot handle the argument sent through the (|>) pipe:"
            , typeComparison localizer argType expectedArgType
                "The argument is:"
                "But (|>) is piping it a function that expects:"
                []
            )

        _ ->
          EmphRight
            ( D.reflow $ "The right side of (|>) needs to be a function so I can pipe arguments to it!"
            , loneType localizer tipe expected
                (D.reflow (addCategory "But instead of a function, I am seeing" category))
                []
            )

    _ ->
      badOpRightFallback localizer category op tipe expected


badOpRightFallback :: L.Localizer -> Category -> N.Name -> T.Type -> T.Type -> RightDocs
badOpRightFallback localizer category op tipe expected =
  EmphRight
    ( D.reflow $
        "The right argument of (" <> N.toString op <> ") is causing problems."
    , typeComparison localizer tipe expected
        (addCategory "The right argument is" category)
        ("But (" <> N.toString op <> ") needs the right argument to be:")
        [ D.toSimpleHint $
            "With operators like (" ++ N.toString op ++ ") I always check the left\
            \ side first. If it seems fine, I assume it is correct and check the right\
            \ side. So the problem may be in how the left and right arguments interact!"
        ]
    )


isInt :: T.Type -> Bool
isInt tipe =
  case tipe of
    T.Type home name [] ->
      T.isInt home name

    _ ->
      False


isFloat :: T.Type -> Bool
isFloat tipe =
  case tipe of
    T.Type home name [] ->
      T.isFloat home name

    _ ->
      False


isString :: T.Type -> Bool
isString tipe =
  case tipe of
    T.Type home name [] ->
      T.isString home name

    _ ->
      False


isList :: T.Type -> Bool
isList tipe =
  case tipe of
    T.Type home name [_] ->
      T.isList home name

    _ ->
      False



-- BAD CONS


badConsRight :: L.Localizer -> Category -> T.Type -> T.Type -> RightDocs
badConsRight localizer category tipe expected =
  case tipe of
    T.Type home1 name1 [actualElement] | T.isList home1 name1 ->
      case expected of
        T.Type home2 name2 [expectedElement] | T.isList home2 name2 ->
          EmphBoth
            ( D.reflow "I am having trouble with this (::) operator:"
            , typeComparison localizer expectedElement actualElement
                "The left side of (::) is:"
                "But you are trying to put that into a list filled with:"
                ( case expectedElement of
                    T.Type home name [_] | T.isList home name ->
                      [ D.toSimpleHint
                          "Are you trying to append two lists? The (++) operator\
                          \ appends lists, whereas the (::) operator is only for\
                          \ adding ONE element to a list."
                      ]

                    _ ->
                      [ D.reflow
                          "Lists need ALL elements to be the same type though."
                      ]
                )
            )

        _ ->
          badOpRightFallback localizer category "::" tipe expected

    _ ->
      EmphRight
        ( D.reflow "The (::) operator can only add elements onto lists."
        , loneType localizer tipe expected
            (D.reflow (addCategory "The right side is" category))
            [D.fillSep ["But","(::)","needs","a",D.dullyellow "List","on","the","right."]
            ]
        )



-- BAD APPEND


data AppendType
  = ANumber D.Doc D.Doc
  | AString
  | AList
  | AOther


toAppendType :: T.Type -> AppendType
toAppendType tipe =
  case tipe of
    T.Type home name _
      | T.isInt    home name -> ANumber "Int" "String.fromInt"
      | T.isFloat  home name -> ANumber "Float" "String.fromFloat"
      | T.isString home name -> AString
      | T.isList   home name -> AList

    T.FlexSuper T.Number _ -> ANumber "number" "String.fromInt"

    _ -> AOther


badAppendLeft :: L.Localizer -> Category -> T.Type -> T.Type -> (D.Doc, D.Doc)
badAppendLeft localizer category tipe expected =
  case toAppendType tipe of
    ANumber thing stringFromThing ->
      ( D.fillSep
          ["The","(++)","operator","can","append","List","and","String"
          ,"values,","but","not",D.dullyellow thing,"values","like","this:"
          ]
      , D.fillSep
          ["Try","using",D.green stringFromThing,"to","turn","it","into","a","string?"
          ,"Or","put","it","in","[]","to","make","it","a","list?"
          ,"Or","switch","to","the","(::)","operator?"
          ]
      )

    _ ->
      ( D.reflow $
          "The (++) operator cannot append this type of value:"
      , loneType localizer tipe expected
          (D.reflow (addCategory "I am seeing" category))
          [ D.fillSep
              ["But","the","(++)","operator","is","only","for","appending"
              ,D.dullyellow "List","and",D.dullyellow "String","values."
              ,"Maybe","put","this","value","in","[]","to","make","it","a","list?"
              ]
          ]
      )


badAppendRight :: L.Localizer -> Category -> T.Type -> T.Type -> RightDocs
badAppendRight localizer category tipe expected =
  case (toAppendType expected, toAppendType tipe) of
    (AString, ANumber thing stringFromThing) ->
      EmphRight
        ( D.fillSep
            ["I","thought","I","was","appending",D.dullyellow "String","values","here,"
            ,"not",D.dullyellow thing,"values","like","this:"
            ]
        , D.fillSep
            ["Try","using",D.green stringFromThing,"to","turn","it","into","a","string?"]
        )

    (AList, ANumber thing _) ->
      EmphRight
        ( D.fillSep
            ["I","thought","I","was","appending",D.dullyellow "List","values","here,"
            ,"not",D.dullyellow thing,"values","like","this:"
            ]
        , D.reflow "Try putting it in [] to make it a list?"
        )

    (AString, AList) ->
      EmphBoth
        ( D.reflow $
            "The (++) operator needs the same type of value on both sides:"
        , D.fillSep
            ["I","see","a",D.dullyellow "String","on","the","left","and","a"
            ,D.dullyellow "List","on","the","right.","Which","should","it","be?"
            ,"Does","the","string","need","[]","around","it","to","become","a","list?"
            ]
        )

    (AList, AString) ->
      EmphBoth
        ( D.reflow $
            "The (++) operator needs the same type of value on both sides:"
        , D.fillSep
            ["I","see","a",D.dullyellow "List","on","the","left","and","a"
            ,D.dullyellow "String","on","the","right.","Which","should","it","be?"
            ,"Does","the","string","need","[]","around","it","to","become","a","list?"
            ]
        )

    (_,_) ->
      EmphBoth
        ( D.reflow $
            "The (++) operator cannot append these two values:"
        , typeComparison localizer expected tipe
            "I already figured out that the left side of (++) is:"
            (addCategory "But this clashes with the right side, which is" category)
            []
        )



-- BAD MATH


data ThisThenThat = FloatInt | IntFloat


badCast :: N.Name -> ThisThenThat -> RightDocs
badCast op thisThenThat =
  EmphBoth
    ( D.reflow $
        "I need both sides of (" <> N.toString op <> ") to be the exact same type. Both Int or both Float."
    , let
        anInt = ["an", D.dullyellow "Int"]
        aFloat = ["a", D.dullyellow "Float"]
        toFloat = D.green "toFloat"
        round = D.green "round"
      in
      case thisThenThat of
        FloatInt ->
          badCastHelp aFloat anInt round toFloat

        IntFloat ->
          badCastHelp anInt aFloat toFloat round
    )


badCastHelp :: [D.Doc] -> [D.Doc] -> D.Doc -> D.Doc -> D.Doc
badCastHelp anInt aFloat toFloat round =
  D.stack
    [ D.fillSep $
        ["But","I","see"]
        ++ anInt
        ++ ["on","the","left","and"]
        ++ aFloat
        ++ ["on","the","right."]
    , D.fillSep
        ["Use",toFloat,"on","the","left","(or",round,"on"
        ,"the","right)","to","make","both","sides","match!"
        ]
    , D.link "Note" "Read" "implicit-casts" "to learn why Elm does not implicitly convert Ints to Floats."
    ]


badStringAdd :: (D.Doc, D.Doc)
badStringAdd =
  (
    D.fillSep ["I","cannot","do","addition","with",D.dullyellow "String","values","like","this","one:"]
  ,
    D.stack
      [ D.fillSep
          ["The","(+)","operator","only","works","with",D.dullyellow "Int","and",D.dullyellow "Float","values."
          ]
      , D.toFancyHint
          ["Switch","to","the",D.green "(++)","operator","to","append","strings!"
          ]
      ]
  )


badListAdd :: L.Localizer -> Category -> String -> T.Type -> T.Type -> (D.Doc, D.Doc)
badListAdd localizer category direction tipe expected =
  (
    "I cannot do addition with lists:"
  ,
    loneType localizer tipe expected
      (D.reflow (addCategory ("The " <> direction <> " side of (+) is") category))
      [ D.fillSep
          ["But","(+)","only","works","with",D.dullyellow "Int","and",D.dullyellow "Float","values."
          ]
      , D.toFancyHint
          ["Switch","to","the",D.green "(++)","operator","to","append","lists!"
          ]
      ]
  )


badListMul :: L.Localizer -> Category -> String -> T.Type -> T.Type -> (D.Doc, D.Doc)
badListMul localizer category direction tipe expected =
  badMath localizer category "Multiplication" direction "*" tipe expected
    [
      D.toFancyHint
        [ "Maybe", "you", "want"
        , D.green "List.repeat"
        , "to", "build","a","list","of","repeated","values?"
        ]
    ]


badMath :: L.Localizer -> Category -> String -> String -> String -> T.Type -> T.Type -> [D.Doc] -> (D.Doc, D.Doc)
badMath localizer category operation direction op tipe expected otherHints =
  (
    D.reflow $
      operation ++ " does not work with this value:"
  ,
    loneType localizer tipe expected
      (D.reflow (addCategory ("The " <> direction <> " side of (" <> op <> ") is") category))
      ( [ D.fillSep
            ["But","(" <> D.fromString op <> ")","only","works","with"
            ,D.dullyellow "Int","and",D.dullyellow "Float","values."
            ]
        ]
        ++ otherHints
      )
  )


badFDiv :: L.Localizer -> D.Doc -> T.Type -> T.Type -> (D.Doc, D.Doc)
badFDiv localizer direction tipe expected =
  (
    D.reflow $
      "The (/) operator is specifically for floating-point division:"
  ,
    if isInt tipe then
      D.stack
        [ D.fillSep
            ["The",direction,"side","of","(/)","must","be","a"
            ,D.dullyellow "Float" <> ","
            ,"but","I","am","seeing","an",D.dullyellow "Int" <> "."
            ,"I","recommend:"
            ]
        , D.vcat
            [ D.green "toFloat" <> " for explicit conversions     " <> D.black "(toFloat 5 / 2) == 2.5"
            , D.green "(//)   " <> " for integer division         " <> D.black "(5 // 2)        == 2"
            ]
        , D.link "Note" "Read" "implicit-casts" "to learn why Elm does not implicitly convert Ints to Floats."
        ]

    else
      loneType localizer tipe expected
        (D.fillSep
          ["The",direction,"side","of","(/)","must","be","a"
          ,D.dullyellow "Float" <> ",","but","instead","I","am","seeing:"
          ]
        )
        []
  )


badIDiv :: L.Localizer -> D.Doc -> T.Type -> T.Type -> (D.Doc, D.Doc)
badIDiv localizer direction tipe expected =
  (
    D.reflow $
      "The (//) operator is specifically for integer division:"
  ,
    if isFloat tipe then
      D.stack
        [ D.fillSep
            ["The",direction,"side","of","(//)","must","be","an"
            ,D.dullyellow "Int" <> ","
            ,"but","I","am","seeing","a",D.dullyellow "Float" <> "."
            ,"I","recommend","doing","the","conversion","explicitly"
            ,"with","one","of","these","functions:"
            ]
        , D.vcat
            [ D.green "round" <> " 3.5     == 4"
            , D.green "floor" <> " 3.5     == 3"
            , D.green "ceiling" <> " 3.5   == 4"
            , D.green "truncate" <> " 3.5  == 3"
            ]
        , D.link "Note" "Read" "implicit-casts" "to learn why Elm does not implicitly convert Ints to Floats."
        ]
    else
      loneType localizer tipe expected
        ( D.fillSep
            ["The",direction,"side","of","(//)","must","be","an"
            ,D.dullyellow "Int" <> ",","but","instead","I","am","seeing:"
            ]
        )
        []
  )



-- BAD BOOLS


badBool :: L.Localizer -> D.Doc -> D.Doc -> T.Type -> T.Type -> (D.Doc, D.Doc)
badBool localizer op direction tipe expected =
  (
    D.reflow $
      "I am struggling with this boolean operation:"
  ,
    loneType localizer tipe expected
      ( D.fillSep
          ["Both","sides","of","(" <> op <> ")","must","be"
          ,D.dullyellow "Bool","values,","but","the",direction,"side","is:"
          ]
      )
      []
  )



-- BAD COMPARISON


badCompLeft :: L.Localizer -> Category -> String -> String -> T.Type -> T.Type -> (D.Doc, D.Doc)
badCompLeft localizer category op direction tipe expected =
  (
    D.reflow $
      "I cannot do a comparison with this value:"
  ,
    loneType localizer tipe expected
      (D.reflow (addCategory ("The " <> direction <> " side of (" <> op <> ") is") category))
      [ D.fillSep
          ["But","(" <> D.fromString op <> ")","only","works","on"
          ,D.dullyellow "Int" <> ","
          ,D.dullyellow "Float" <> ","
          ,D.dullyellow "Char" <> ","
          ,"and"
          ,D.dullyellow "String"
          ,"values.","It","can","work","on","lists","and","tuples"
          ,"of","comparable","values","as","well,","but","it","is"
          ,"usually","better","to","find","a","different","path."
          ]
      ]
  )


badCompRight :: L.Localizer -> String -> T.Type -> T.Type -> RightDocs
badCompRight localizer op tipe expected =
  EmphBoth
    (
      D.reflow $
        "I need both sides of (" <> op <> ") to be the same type:"
    ,
      typeComparison localizer expected tipe
        ("The left side of (" <> op <> ") is:")
        "But the right side is:"
        [ D.reflow $
            "I cannot compare different types though! Which side of (" <> op <> ") is the problem?"
        ]
    )



-- BAD EQUALITY


badEquality :: L.Localizer -> String -> T.Type -> T.Type -> RightDocs
badEquality localizer op tipe expected =
  EmphBoth
    (
      D.reflow $
        "I need both sides of (" <> op <> ") to be the same type:"
    ,
      typeComparison localizer expected tipe
        ("The left side of (" <> op <> ") is:")
        "But the right side is:"
        [ if isFloat tipe || isFloat expected then
            D.toSimpleNote $
              "Equality on floats is not 100% reliable due to the design of IEEE 754. I\
              \ recommend a check like (abs (x - y) < 0.0001) instead."
          else
            D.reflow  "Different types can never be equal though! Which side is messed up?"
        ]
    )



-- INFINITE TYPES


toInfiniteReport :: Code.Source -> L.Localizer -> R.Region -> N.Name -> T.Type -> Report.Report
toInfiniteReport source localizer region name overallType =
  Report.Report "INFINITE TYPE" region [] $
    Report.toCodeSnippet source region Nothing
      (
        D.reflow $
          "I am inferring a weird self-referential type for " <> N.toString name <> ":"
      ,
        D.stack
          [ D.reflow $
              "Here is my best effort at writing down the type. You will see ∞ for\
              \ parts of the type that repeat something already printed out infinitely."
          , D.indent 4 (D.dullyellow (T.toDoc localizer RT.None overallType))
          , D.reflowLink
              "Staring at this type is usually not so helpful, so I recommend reading the hints at"
              "infinite-type"
              "to get unstuck!"
          ]
      )
