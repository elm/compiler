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
import Data.Monoid ((<>))

import qualified Data.Index as Index
import qualified Elm.Name as N
import qualified Reporting.Helpers as H
import qualified Reporting.Region as R
import qualified Reporting.Render.Code as Code
import qualified Reporting.Render.Type as RT
import qualified Reporting.Report as Report
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
  | CallArg MaybeName Index.ZeroBased
  | TooManyArgs MaybeName Int
  | RecordAccess N.Name
  | RecordUpdate
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


toReport :: Code.Source -> T.Localizer -> Error -> Report.Report
toReport source localizer err =
  case err of
    BadExpr region category actualType expected ->
      toExprReport source localizer region category actualType expected

    BadPattern region category tipe expected ->
      toPatternReport source localizer region category tipe expected

    InfiniteType region name overallType ->
      toInfiniteReport source localizer region name overallType



-- TO PATTERN REPORT


toPatternReport :: Code.Source -> T.Localizer -> R.Region -> PCategory -> T.Type -> PExpected T.Type -> Report.Report
toPatternReport source localizer patternRegion category tipe expected =
  Report.Report "TYPE MISMATCH" patternRegion [] $
  case expected of
    PNoExpectation expectedType ->
      Report.toCodeSnippet source patternRegion Nothing $
        (
          "This pattern is being used in an unexpected way:"
        ,
          patternTypeComparison localizer tipe expectedType
            (
              toPatternDescription category "It is"
            ,
              "But it needs to match values of type:"
            ,
              []
            )
        )

    PFromContext region context expectedType ->
      Report.toCodeSnippet source region (Just patternRegion) $
        case context of
          PTypedArg name index ->
            (
              H.reflow $
                "The " <> H.ordinalize (Index.toHuman index) <> " argument to `" <> N.toString name <> "` is weird."
            ,
              patternTypeComparison localizer tipe expectedType
                (
                  toPatternDescription category $
                    "The argument is a pattern that matches"
                ,
                  "But the type annotation on `" <> N.toString name <> "` says the " <> H.ordinalize (Index.toHuman index) <> " argument should be:"
                ,
                  []
                )
            )

          PCaseMatch index ->
            if index == Index.first then
              (
                H.reflow $
                  "The 1st pattern in this `case` causing a mismatch:"
              ,
                patternTypeComparison localizer tipe expectedType
                  (
                    toPatternDescription category "The first pattern is trying to match"
                  ,
                    "But it is supposed to match an expression of type:"
                  ,
                    [ H.reflow $
                        "Is the pattern the problem? Or is it the expression you are trying to match on?"
                    ]
                  )
              )
            else
              (
                H.reflow $
                  "The " <> H.ordinalize (Index.toHuman index) <> " pattern in this `case` does not match the previous ones."
              ,
                patternTypeComparison localizer tipe expectedType
                  (
                    toPatternDescription category $
                      "The " <> H.ordinalize (Index.toHuman index) <> " pattern is trying to match"
                  ,
                    "But all the previous patterns match values of type:"
                  ,
                    []
                  )
              )

          PCtorArg name index ->
            (
              H.reflow $
                "The " <> H.ordinalize (Index.toHuman index) <> " argument to `" <> N.toString name <> "` is weird."
            ,
              patternTypeComparison localizer tipe expectedType
                (
                  toPatternDescription category "It is trying to match"
                ,
                  "But `" <> N.toString name <> "` needs its " <> H.ordinalize (Index.toHuman index) <> " argument to be:"
                ,
                  []
                )
            )

          PListEntry index ->
            (
              H.reflow $
                "The " <> H.ordinalize (Index.toHuman index) <> " pattern in this list does not match all the previous ones:"
            ,
              patternTypeComparison localizer tipe expectedType
                (
                  toPatternDescription category $
                    "The " <> H.ordinalize (Index.toHuman index) <> " pattern is trying to match"
                ,
                  "But all the previous patterns in the list are:"
                ,
                  [ H.toSimpleHint $
                      "Everything in the list needs to be the same type of value.\
                      \ This way you never run into unexpected values partway through.\
                      \ To mix different types in a single list, create a \"union type\" as\
                      \ described in: <http://guide.elm-lang.org/types/union_types.html>"
                  ]
                )
            )

          PTail ->
            (
              H.reflow $
                "The pattern after (::) is causing issues."
            ,
              patternTypeComparison localizer tipe expectedType
                (
                  toPatternDescription category $
                    "The pattern after (::) is trying to match"
                ,
                  "But it needs to match lists like this:"
                ,
                  []
                )
            )



-- PATTERN HELPERS


patternTypeComparison :: T.Localizer -> T.Type -> T.Type -> ( String, String, [H.Doc] ) -> H.Doc
patternTypeComparison localizer actual expected ( iAmSeeing, insteadOf, contextHints ) =
  let
    (actualDoc, expectedDoc, problems) =
      T.toDiffDocs localizer actual expected
  in
  H.stack $
    [ H.reflow iAmSeeing
    , H.indent 4 actualDoc
    , H.reflow insteadOf
    , H.indent 4 expectedDoc
    ]
    ++ problemsToHint problems
    ++ contextHints


toPatternDescription :: PCategory -> String -> String
toPatternDescription category iAmTryingToMatch =
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



-- EXPR HELPERS


typeComparison :: T.Localizer -> T.Type -> T.Type -> ( String, String, [H.Doc] ) -> H.Doc
typeComparison localizer actual expected ( iAmSeeing, insteadOf, contextHints ) =
  let
    (actualDoc, expectedDoc, problems) =
      T.toDiffDocs localizer actual expected
  in
  H.stack $
    [ H.reflow iAmSeeing
    , H.indent 4 actualDoc
    , H.reflow insteadOf
    , H.indent 4 expectedDoc
    ]
    ++ problemsToHint problems
    ++ contextHints


loneType :: T.Localizer -> T.Type -> T.Type -> ( H.Doc, [H.Doc] ) -> H.Doc
loneType localizer actual expected ( iAmSeeing, furtherDetails ) =
  let
    (actualDoc, _, problems) =
      T.toDiffDocs localizer actual expected
  in
  H.stack $
    [ iAmSeeing
    , H.indent 4 actualDoc
    ]
    ++ furtherDetails
    ++ problemsToHint problems


toDescription :: Category -> String -> String
toDescription category thatThingIs =
  case category of
    Local name -> "This `" <> N.toString name <> "` value has type:"
    Foreign name -> "This `" <> N.toString name <> "` value has type:"
    Access field -> "The value at ." <> N.toString field <> " has type:"
    Accessor field -> "This ." <> N.toString field <> " field access function has type:"
    If -> "This `if` expression produces a value of type:"
    Case -> "This `case` expression produces a value of type:"
    List -> thatThingIs <> " a list of type:"
    Number -> thatThingIs <> " a number of type:"
    Float -> thatThingIs <> " a float of type:"
    String -> thatThingIs <> " a string of type:"
    Char -> thatThingIs <> " a character of type:"
    Lambda -> thatThingIs <> " an anonymous function of type:"
    Record -> thatThingIs <> " a record of type:"
    Tuple -> thatThingIs <> " a tuple of type:"
    Unit -> thatThingIs <> " a unit value:"
    Shader -> thatThingIs <> " a GLSL shader of type:"
    Effects -> thatThingIs <> " a thing for CORE LIBRARIES ONLY."
    CallResult maybeName ->
      case maybeName of
        NoName -> thatThingIs <> ":"
        FuncName name -> "This `" <> N.toString name <> "` call produces:"
        CtorName name -> "This `" <> N.toString name <> "` call produces:"
        OpName _ -> thatThingIs <> ":"


problemsToHint :: [T.Problem] -> [H.Doc]
problemsToHint problems =
  case problems of
    [] ->
      []

    problem : _ ->
      problemToHint problem


problemToHint :: T.Problem -> [H.Doc]
problemToHint problem =
  case problem of
    T.FieldMismatch _ _ -> [] -- TODO do a better job?

    T.IntFloat ->
      [ H.fancyLink "Note" ["Read"] "implicit-casts"
          ["to","learn","why","Elm","does","not","implicitly","convert"
          ,"Ints","to","Floats.","Use",H.green "toFloat","and"
          ,H.green "round","to","do","explicit","conversions."
          ]
      ]

    T.StringFromInt ->
      [ H.toFancyHint
          ["Want","to","convert","an","Int","into","a","String?"
          ,"Use","the",H.green "String.fromInt","function!"
          ]
      ]

    T.StringFromFloat ->
      [ H.toFancyHint
          ["Want","to","convert","a","Float","into","a","String?"
          ,"Use","the",H.green "String.fromFloat","function!"
          ]
      ]

    T.StringToInt ->
      [ H.toFancyHint
          ["Want","to","convert","a","String","into","an","Int?"
          ,"Use","the",H.green "String.toInt","function!"
          ]
      ]

    T.StringToFloat ->
      [ H.toFancyHint
          ["Want","to","convert","a","String","into","a","Float?"
          ,"Use","the",H.green "String.toFloat","function!"
          ]
      ]

    T.AnythingToBool ->
      [ H.toSimpleHint $
          "Elm does not have “truthiness” such that ints and strings and lists\
          \ are automatically converted to booleans. Do that conversion explicitly!"
      ]

    T.AnythingFromMaybe ->
      [ H.toFancyHint
          ["Use",H.green "Maybe.withDefault","to","handle","possible","errors."
          ,"Longer","term,","it","is","usually","better","to","write","out","the"
          ,"full","`case`","though!"
          ]
      ]

    T.AnythingToList ->
      [ H.toSimpleHint "Did you forget to add [] around it?"
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



-- BAD RIGID HINTS


badRigidVar :: N.Name -> String -> [H.Doc]
badRigidVar name aThing =
  [ H.toSimpleHint $
      "Your type annotation uses type variable `" ++ N.toString name ++
      "` which means ANY type of value can flow through, but your code is saying it specifically wants "
      ++ aThing ++ ". Maybe change your type annotation to\
      \ be more specific? Maybe change the code to be more general?"
  , H.reflowLink "Read" "type-annotations" "for more advice!"
  ]


badDoubleRigid :: N.Name -> N.Name -> [H.Doc]
badDoubleRigid x y =
  [ H.toSimpleHint $
      "Your type annotation uses `" ++ N.toString x ++ "` and `" ++ N.toString y ++
      "` as separate type variables. Your code seems to be saying they are the\
      \ same though! Maybe they should be the same in your type annotation?\
      \ Maybe your code uses them in a weird way?"
  , H.reflowLink "Read" "type-annotations" "for more advice!"
  ]


toASuperThing :: T.Super -> String
toASuperThing super =
  case super of
    T.Number     -> "a `number` value"
    T.Comparable -> "a `comparable` value"
    T.CompAppend -> "a `compappend` value"
    T.Appendable -> "an `appendable` value"



-- BAD SUPER HINTS


badFlexSuper :: T.Super -> T.Type -> [H.Doc]
badFlexSuper super tipe =
  case super of
    T.Comparable ->
      [ H.toSimpleHint "Only ints, floats, chars, strings, lists, and tuples are comparable."
      ]

    T.Appendable ->
      case tipe of
        T.Type home name _ | T.isInt home name ->
          [ H.toFancyHint ["Try","using",H.green"String.fromInt","to","convert","it","to","a","string?"]
          ]

        T.Type home name _ | T.isFloat home name ->
          [ H.toFancyHint ["Try","using",H.green"String.fromFloat","to","convert","it","to","a","string?"]
          ]

        T.FlexSuper T.Number _ ->
          [ H.toFancyHint ["Try","using",H.green"String.fromInt","to","convert","it","to","a","string?"]
          ]

        _ ->
          [ H.toFancyHint ["Only","strings","and","lists","are","appendable.","Put","it","in",H.green "[]","to","make","it","a","list?"]
          ]

    T.CompAppend ->
      [ H.toSimpleHint "Only strings and lists are both comparable and appendable."
      ]

    T.Number ->
      case tipe of
        T.Type home name _ | T.isString home name ->
          [ H.toFancyHint ["Try","using",H.green"String.toInt","to","convert","it","to","a","number?"]
          ]

        _ ->
          [ H.toFancyHint ["Only",H.green "Int","and",H.green "Float","values","work","as","numbers."]
          ]


badRigidSuper :: T.Super -> String -> [H.Doc]
badRigidSuper super aThing =
  let
    (superType, manyThings) =
      case super of
        T.Number -> ("number", "ints AND floats")
        T.Comparable -> ("comparable", "ints, floats, chars, strings, lists, and tuples")
        T.Appendable -> ("appendable", "strings AND lists")
        T.CompAppend -> ("compappend", "strings AND lists")
  in
  [ H.toSimpleHint $
      "The `" ++ superType ++ "` in your type annotation is saying that "
      ++ manyThings ++ " can flow through, but your code is saying it specifically wants "
      ++ aThing ++ ". Maybe change your type annotation to\
      \ be more specific? Maybe change the code to be more general?"
  , H.reflowLink "Read" "type-annotations" "for more advice!"
  ]


badFlexFlexSuper :: T.Super -> T.Super -> [H.Doc]
badFlexFlexSuper s1 s2 =
  let
    likeThis super =
      case super of
        T.Number -> "a number"
        T.Comparable -> "comparable"
        T.CompAppend -> "a compappend"
        T.Appendable -> "appendable"
  in
    [ H.toSimpleHint $
        "There are no values in Elm that are both"
        ++ likeThis s1 ++ " and " ++ likeThis s2 ++ "."
    ]



-- TO EXPR REPORT


toExprReport :: Code.Source -> T.Localizer -> R.Region -> Category -> T.Type -> Expected T.Type -> Report.Report
toExprReport source localizer exprRegion category tipe expected =
  Report.Report "TYPE MISMATCH" exprRegion [] $
  case expected of
    NoExpectation expectedType ->
      Report.toCodeSnippet source exprRegion Nothing
        (
          "This expression is being used in an unexpected way:"
        ,
          typeComparison localizer tipe expectedType
            (
              toDescription category "It is"
            ,
              "But you are trying to use it as:"
            ,
              []
            )
        )

    FromAnnotation name _arity subContext expectedType ->
      Report.toCodeSnippet source exprRegion Nothing
        (
          H.reflow $
            case subContext of
              TypedIfBranch index ->
                "Something is off with the " <> H.ordinalize (Index.toHuman index) <> " branch of this `if` expression:"

              TypedCaseBranch index ->
                "Something is off with the " <> H.ordinalize (Index.toHuman index) <> " branch of this `case` expression:"

              TypedBody ->
                "Something is off with the body of the `" <> N.toString name <> "` definition:"
        ,
          typeComparison localizer tipe expectedType
            (
              toDescription category $
                case subContext of
                  TypedIfBranch index ->
                    "The " <> H.ordinalize (Index.toHuman index) <> " branch is"

                  TypedCaseBranch index ->
                    "The " <> H.ordinalize (Index.toHuman index) <> " branch is"

                  TypedBody ->
                    "The body is"
            ,
              "But the type annotation on `" <> N.toString name <> "` says it should be:"
            ,
              []
            )
        )

    FromContext region context expectedType ->
      case context of
        ListEntry index ->
          Report.toCodeSnippet source region (Just exprRegion)
            (
              H.reflow $
                "The " <> H.ordinalize (Index.toHuman index) <> " element of this list does not match all the previous elements:"
            ,
              typeComparison localizer tipe expectedType
                (
                  toDescription category $
                    "The " <> H.ordinalize (Index.toHuman index) <> " element is"
                ,
                  "But all the previous elements in the list are:"
                ,
                  [ H.toSimpleHint $
                      "Everything in the list needs to be the same type of value.\
                      \ This way you never run into unexpected values partway through.\
                      \ To mix different types in a single list, create a \"union type\" as\
                      \ described in: <http://guide.elm-lang.org/types/union_types.html>"
                  ]
                )
            )

        Negate ->
          Report.toCodeSnippet source region (Just exprRegion)
            (
              "I do not know how to negate this type of value:"
            ,
              loneType localizer tipe expectedType
                (
                  H.reflow $ toDescription category "It is"
                ,
                  [ H.fillSep
                      ["But","I","only","now","how","to","negate"
                      ,H.dullyellow "Int","and",H.dullyellow "Float","values."
                      ]
                  ]
                )
            )

        OpLeft op ->
          Report.toCodeSnippet source region (Just exprRegion) $
            opLeftToDocs localizer category op tipe expectedType

        OpRight op ->
          case opRightToDocs localizer category op tipe expectedType of
            EmphBoth details ->
              Report.toCodeSnippet source region Nothing details

            EmphRight details ->
              Report.toCodeSnippet source region (Just exprRegion) details

        IfCondition ->
          Report.toCodeSnippet source region (Just exprRegion)
            (
              "This `if` condition does not evaluate to a boolean value, True or False."
            ,
              loneType localizer tipe expectedType
                (
                  H.reflow $ toDescription category "It is"
                ,
                  [ H.fillSep ["But","I","need","this","`if`","condition","to","be","a",H.dullyellow "Bool","value."]
                  ]
                )
            )

        IfBranch index ->
          Report.toCodeSnippet source region (Just exprRegion)
            (
              H.reflow $
                "The " <> H.ordinalize (Index.toHuman index) <> " branch of this `if` does not match all the previous branches:"
            ,
              typeComparison localizer tipe expectedType
                (
                  toDescription category $
                    "The " <> H.ordinalize (Index.toHuman index) <> " branch is"
                ,
                  "But all the previous branches result in:"
                ,
                  [ H.link "Hint"
                      "All branches in an `if` must produce the same type of values. This way, no\
                      \ matter which branch we take, the result is always a consistent shape. Read"
                      "union-types"
                      "to learn how to “mix” types."
                  ]
                )
            )

        CaseBranch index ->
          Report.toCodeSnippet source region (Just exprRegion)
            (
              H.reflow $
                "The " <> H.ordinalize (Index.toHuman index) <> " branch of this `case` does not match all the previous branches:"
            ,
              typeComparison localizer tipe expectedType
                (
                  toDescription category $
                    "The " <> H.ordinalize (Index.toHuman index) <> " branch is"
                ,
                  "But all the previous branches result in:"
                ,
                  [ H.link "Hint"
                      "All branches in a `case` must produce the same type of values. This way, no\
                      \ matter which branch we take, the result is always a consistent shape. Read"
                      "union-types"
                      "to learn how to “mix” types."
                  ]
                )
            )

        CallArg maybeFuncName index ->
          let
            thisFunction =
              case maybeFuncName of
                NoName ->
                  "this function"

                FuncName name ->
                  "`" <> N.toString name <> "`"

                CtorName name ->
                  "`" <> N.toString name <> "`"

                OpName op ->
                  "(" <> N.toString op <> ")"

          in
          Report.toCodeSnippet source region (Just exprRegion)
            (
              H.reflow $
                "The " <> H.ordinalize (Index.toHuman index) <> " argument to " <> thisFunction <> " is not what I expect:"
            ,
              typeComparison localizer tipe expectedType
                (
                  toDescription category "This argument is"
                ,
                  "But " <> thisFunction <> " needs the " <> H.ordinalize (Index.toHuman index)
                  <> " argument to be:"
                ,
                  if Index.toHuman index == 1 then
                    []
                  else
                    [ H.toSimpleHint $
                       "I always figure out the argument types from left to right. If an argument\
                        \ is acceptable, I assume it is “correct” and move on. So the problem may\
                        \ actually be in one of the previous arguments!"
                    ]
                )
            )

        TooManyArgs maybeName numGivenArgs ->
          case countArgs tipe of
            0 ->
              Report.toCodeSnippet source region (Just exprRegion)
                (
                  H.reflow $
                    case maybeName of
                      NoName ->
                        "You are giving " <> H.args numGivenArgs <> " to something that is not a function!"

                      FuncName name ->
                        "`" <> N.toString name <> "` is not a function, but you are giving it " <> H.args numGivenArgs <> "!"

                      CtorName name ->
                        "`" <> N.toString name <> "` is not a function, but you are giving it " <> H.args numGivenArgs <> "!"

                      OpName op ->
                        "(" <> N.toString op <> ") is not a function, but you are giving it " <> H.args numGivenArgs <> "!"
                ,
                  typeComparison localizer tipe expectedType
                    (
                      case maybeName of
                        NoName ->
                          "It is a value of type:"

                        FuncName name ->
                          "`" <> N.toString name <> "` is a value of type:"

                        CtorName name ->
                          "`" <> N.toString name <> "` is a constructor of type:"

                        OpName op ->
                          "(" <> N.toString op <> ") is a value of type:"
                    ,
                      "But you are giving it " <> H.args numGivenArgs <> " as if its type was:"
                    ,
                      [ "Maybe you forgot some parentheses? Or a comma?"
                      ]
                    )
                )

            numExpectedArgs ->
              let
                thisFunction =
                  case maybeName of
                    NoName ->
                      "This function"

                    FuncName name ->
                      "The `" <> N.toString name <> "` function"

                    CtorName name ->
                      "The `" <> N.toString name <> "` constructor"

                    OpName op ->
                      "The (" <> N.toString op <> ") function"
              in
              Report.toCodeSnippet source region (Just exprRegion)
                (
                  H.reflow $
                    thisFunction <> " is getting too many arguments:"
                ,
                  typeComparison localizer tipe expectedType
                    (
                      thisFunction <> " is expecting to get " <> H.args numExpectedArgs <> " like this:"
                    ,
                      "But you are giving it " <> H.args numGivenArgs <> " as if its type was:"
                    ,
                      [ "Maybe you forgot some parentheses? Or a comma?"
                      , H.toSimpleNote $
                          "I am not good at figuring out which arguments are extra myself. The\
                          \ problem may even be that the FUNCTION is defined funny, and the arguments are\
                          \ actually fine. Anyway, I hope I gave enough info to help you figure it out!"
                      ]
                    )
                )

        RecordAccess field ->
          Report.toCodeSnippet source region (Just exprRegion) $
            case tipe of
              T.Record _ _ ->
                (
                  H.reflow $
                    "This record does not have a `" <> N.toString field <> "` field:"
                ,
                  loneType localizer tipe expectedType
                    (
                      H.reflow $ "It is a record with this type:"
                    ,
                      [ H.fillSep
                          ["Is","the",H.dullyellow ("." <> H.nameToDoc field)
                          ,"field","access","is","misspelled?"
                          ]
                      ]
                    )
                )

              _ ->
                (
                  H.reflow $
                    "This is not a record, so it has no fields to access!"
                ,
                  loneType localizer tipe expectedType
                    ( H.reflow $ toDescription category "It is"
                    , [ H.fillSep
                          ["But","I","need","a","record","with","a"
                          ,H.dullyellow (H.nameToDoc field),"field!"
                          ]
                      ]
                    )
                )

        RecordUpdate ->
          case tipe of
            T.Record _ _ ->
              Report.toCodeSnippet source region Nothing $
                (
                  H.reflow $
                    "I think there is a typo in one of these field names:"
                ,
                  typeComparison localizer tipe expectedType
                    (
                      "The record you want to update has type:"
                    ,
                      "But you are saying it should have these fields:"
                    ,
                      [ H.reflow "Maybe there is some typo?"
                      ]
                    )
                )

            _ ->
              Report.toCodeSnippet source region (Just exprRegion)
                (
                  "The record update syntax does not work with this value:"
                ,
                  loneType localizer tipe expectedType
                    ( H.reflow $ toDescription category "It is"
                    , [ H.reflow $ "But I need some kind of record here!"
                      ]
                    )
                )

        Destructure ->
          Report.toCodeSnippet source region Nothing
            (
              H.reflow $
                "I cannot destructure "
            ,
              typeComparison localizer tipe expectedType
                (
                  toDescription category "You are defining"
                ,
                  "But then trying to destructure it as:"
                ,
                  []
                )
            )



-- COUNT ARGS


countArgs :: T.Type -> Int
countArgs tipe =
  case tipe of
    T.Lambda _ _ stuff ->
      1 + length stuff

    _ ->
      0



-- OP LEFT


opLeftToDocs :: T.Localizer -> Category -> N.Name -> T.Type -> T.Type -> (H.Doc, H.Doc)
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
      ,
        loneType localizer tipe expected
          ( H.reflow $ toDescription category "I am seeing"
          , [ H.reflow $ "This needs to be some kind of function though!"
            ]
          )
      )

    _ ->
      (
        H.reflow $
          "The left argument of (" <> N.toString op <> ") is causing problems:"
      ,
        typeComparison localizer tipe expected
          (
            toDescription category "The left argument is"
          ,
            "But (" <> N.toString op <> ") needs the left argument to be:"
          ,
            []
          )
      )



-- OP RIGHT


data RightDocs
  = EmphBoth (H.Doc, H.Doc)
  | EmphRight (H.Doc, H.Doc)


opRightToDocs :: T.Localizer -> Category -> N.Name -> T.Type -> T.Type -> RightDocs
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
        (
          H.reflow $
            "I cannot send this through the (<|) pipe:"
        ,
          typeComparison localizer tipe expected
            (
              toDescription category "You are providing"
            ,
              "But (<|) is piping it a function that expects:"
            ,
              []
            )
        )

    "|>" ->
      case (tipe, expected) of
        (T.Lambda expectedArgType _ _, T.Lambda argType _ _) ->
          EmphRight
            (
              "This function cannot handle the argument sent through the (|>) pipe:"
            ,
              typeComparison localizer argType expectedArgType
                (
                  "You are providing an argument of type:"
                ,
                  "But (|>) is piping it a function that expects its next argument to be:"
                ,
                  []
                )
            )

        _ ->
          EmphRight
            (
              "The right side of (|>) needs to be a function so I can pipe arguments to it!"
            ,
              loneType localizer tipe expected
                ( H.reflow $ toDescription category $
                    "But instead of a function, I am seeing "
                , []
                )
            )

    _ ->
      badOpRightFallback localizer category op tipe expected


badOpRightFallback :: T.Localizer -> Category -> N.Name -> T.Type -> T.Type -> RightDocs
badOpRightFallback localizer category op tipe expected =
  EmphRight
    (
      H.reflow $
        "The right argument of (" <> N.toString op <> ") is causing problems."
    ,
      typeComparison localizer tipe expected
        (
          toDescription category "The right argument is"
        ,
          "But (" <> N.toString op <> ") needs the right argument to be:"
        ,
          [ H.toSimpleHint $
              "With operators like (" ++ N.toString op ++ ") I always check the left\
              \ side first. If it seems fine, I assume it is correct and check the right\
              \ side. So the problem may be in how the left and right arguments interact!"
          ]
        )
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


badConsRight :: T.Localizer -> Category -> T.Type -> T.Type -> RightDocs
badConsRight localizer category tipe expected =
  case tipe of
    T.Type home1 name1 [actualElement] | T.isList home1 name1 ->
      case expected of
        T.Type home2 name2 [expectedElement] | T.isList home2 name2 ->
          EmphBoth
            (
              H.reflow "I am having trouble with this (::) operator:"
            ,
              typeComparison localizer expectedElement actualElement
                (
                  "The left side of (::) is an element of type:"
                ,
                  "But the right side is a list filled with elements of type:"
                ,
                  case expectedElement of
                    T.Type home name [_] | T.isList home name ->
                      [ H.toSimpleHint
                          "Are you trying to append two lists? The (++) operator\
                          \ appends lists, whereas the (::) operator is only for\
                          \ adding ONE element to a list."
                      ]

                    _ ->
                      [ H.reflow
                          "Lists need ALL elements to be the same type though."
                      ]
                )
            )

        _ ->
          badOpRightFallback localizer category "::" tipe expected

    _ ->
      EmphRight
        (
          H.reflow "The (::) operator can only add elements onto lists."
        ,
          loneType localizer tipe expected
            (
              H.reflow $ toDescription category "The right side is"
            ,
              [H.fillSep ["But","(::)","needs","a",H.dullyellow "List","on","the","right."]
              ]
            )
        )



-- BAD APPEND


data AppendType
  = ANumber H.Doc H.Doc
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


badAppendLeft :: T.Localizer -> Category -> T.Type -> T.Type -> (H.Doc, H.Doc)
badAppendLeft localizer category tipe expected =
  case toAppendType tipe of
    ANumber thing stringFromThing ->
      (
        H.fillSep
          ["The","(++)","operator","can","append","List","and","String"
          ,"values,","but","not",H.dullyellow thing,"values","like","this:"
          ]
      ,
        H.fillSep
          ["Try","using",H.green stringFromThing,"to","turn","it","into","a","string?"
          ,"Or","put","it","in","[]","to","make","it","a","list?"
          ,"Or","switch","to","the","(::)","operator?"
          ]
      )

    _ ->
      (
        H.reflow $
          "The (++) operator cannot append this type of value:"
      ,
        loneType localizer tipe expected
          ( H.reflow $ toDescription category "I am seeing"
          ,
            [ H.fillSep
                ["But","the","(++)","operator","is","only","for","appending"
                ,H.dullyellow "List","and",H.dullyellow "String","values."
                ,"Maybe","put","this","value","in","[]","to","make","it","a","list?"
                ]
            ]
          )
      )


badAppendRight :: T.Localizer -> Category -> T.Type -> T.Type -> RightDocs
badAppendRight localizer category tipe expected =
  case (toAppendType expected, toAppendType tipe) of
    (AString, ANumber thing stringFromThing) ->
      EmphRight
        (
          H.fillSep
            ["I","thought","I","was","appending",H.dullyellow "String","values","here,"
            ,"not",H.dullyellow thing,"values","like","this:"
            ]
        ,
          H.fillSep
            ["Try","using",H.green stringFromThing
            ,"to","turn","it","into","a","string?"
            ]
        )

    (AList, ANumber thing _) ->
      EmphRight
        (
          H.fillSep
            ["I","thought","I","was","appending",H.dullyellow "List","values","here,"
            ,"not",H.dullyellow thing,"values","like","this:"
            ]
        ,
          H.reflow "Try putting it in [] to make it a list?"
        )

    (AString, AList) ->
      EmphBoth
        (
          H.reflow $
            "The (++) operator needs the same type of value on both sides:"
        ,
          H.fillSep
            ["I","see","a",H.dullyellow "String","on","the","left","and","a"
            ,H.dullyellow "List","on","the","right.","Which","should","it","be?"
            ,"Does","the","string","need","[]","around","it","to","become","a","list?"
            ]
        )

    (AList, AString) ->
      EmphBoth
        (
          H.reflow $
            "The (++) operator needs the same type of value on both sides:"
        ,
          H.fillSep
            ["I","see","a",H.dullyellow "List","on","the","left","and","a"
            ,H.dullyellow "String","on","the","right.","Which","should","it","be?"
            ,"Does","the","string","need","[]","around","it","to","become","a","list?"
            ]
        )

    (_,_) ->
      EmphBoth
        (
          H.reflow $
            "The (++) operator cannot append these two values:"
        ,
          typeComparison localizer expected tipe
            (
              "I already figured out that the left side of (++) is:"
            ,
              toDescription category $
                "But this clashes with the right side, which is"
            ,
              []
            )
        )



-- BAD MATH


data ThisThenThat = FloatInt | IntFloat


badCast :: N.Name -> ThisThenThat -> RightDocs
badCast op thisThenThat =
  EmphBoth
    (
      H.text $
        "I need both sides of (" <> N.toString op <> ") to be the exact same type. Both Int or both Float."
    ,
      let
        anInt = ["an", H.dullyellow "Int"]
        aFloat = ["a", H.dullyellow "Float"]
        toFloat = H.green "toFloat"
        round = H.green "round"
      in
      case thisThenThat of
        FloatInt ->
          badCastHelp aFloat anInt round toFloat

        IntFloat ->
          badCastHelp anInt aFloat toFloat round
    )


badCastHelp :: [H.Doc] -> [H.Doc] -> H.Doc -> H.Doc -> H.Doc
badCastHelp anInt aFloat toFloat round =
  H.stack
    [ H.fillSep $
        ["But","I","see"]
        ++ anInt
        ++ ["on","the","left","and"]
        ++ aFloat
        ++ ["on","the","right."]
    , H.fillSep
        ["Use",toFloat,"on","the","left","(or",round,"on"
        ,"the","right)","to","make","both","sides","match!"
        ]
    , H.link "Note" "Read" "implicit-casts" "to learn why Elm does not implicitly convert Ints to Floats."
    ]


badStringAdd :: (H.Doc, H.Doc)
badStringAdd =
  (
    H.fillSep ["I","cannot","do","addition","with",H.dullyellow "String","values","like","this","one:"]
  ,
    H.stack
      [ H.fillSep
          ["The","(+)","operator","only","works","with",H.dullyellow "Int","and",H.dullyellow "Float","values."
          ]
      , H.toFancyHint
          ["Switch","to","the",H.green "(++)","operator","to","append","strings!"
          ]
      ]
  )


badListAdd :: T.Localizer -> Category -> String -> T.Type -> T.Type -> (H.Doc, H.Doc)
badListAdd localizer category direction tipe expected =
  (
    "I cannot do addition with lists:"
  ,
    loneType localizer tipe expected
      (
        H.reflow $ toDescription category $
          "The " <> direction <> " side of (+) is"
      ,
        [ H.fillSep
            ["But","(+)","only","works","with",H.dullyellow "Int","and",H.dullyellow "Float","values."
            ]
        , H.toFancyHint
            ["Switch","to","the",H.green "(++)","operator","to","append","lists!"
            ]
        ]
      )
  )


badListMul :: T.Localizer -> Category -> String -> T.Type -> T.Type -> (H.Doc, H.Doc)
badListMul localizer category direction tipe expected =
  badMath localizer category "Multiplication" direction "*" tipe expected
    [
      H.toFancyHint
        [ "Maybe", "you", "want"
        , H.green "List.repeat"
        , "to", "build","a","list","of","repeated","values?"
        ]
    ]


badMath :: T.Localizer -> Category -> String -> String -> String -> T.Type -> T.Type -> [H.Doc] -> (H.Doc, H.Doc)
badMath localizer category operation direction op tipe expected otherHints =
  (
    H.reflow $
      operation ++ " does not work with this value:"
  ,
    loneType localizer tipe expected
      ( H.reflow $ toDescription category $
          "The " <> direction <> " side of (" <> op <> ") is"
      ,
        [ H.fillSep
            ["But","(" <> H.text op <> ")","only","works","with"
            ,H.dullyellow "Int","and",H.dullyellow "Float","values."
            ]
        ]
        ++ otherHints
      )
  )


badFDiv :: T.Localizer -> H.Doc -> T.Type -> T.Type -> (H.Doc, H.Doc)
badFDiv localizer direction tipe expected =
  (
    H.reflow $
      "The (/) operator is specifically for floating-point division:"
  ,
    if isInt tipe then
      H.stack
        [ H.fillSep
            ["The",direction,"side","of","(/)","must","be","a"
            ,H.dullyellow "Float" <> ","
            ,"but","I","am","seeing","an",H.dullyellow "Int" <> "."
            ,"I","recommend:"
            ]
        , H.vcat
            [ H.green "toFloat" <> " for explicit conversions     " <> H.black "(toFloat 5 / 2) == 2.5"
            , H.green "(//)   " <> " for integer division         " <> H.black "(5 // 2)        == 2"
            ]
        , H.link "Note" "Read" "implicit-casts" "to learn why Elm does not implicitly convert Ints to Floats."
        ]

    else
      loneType localizer tipe expected
        (
          H.fillSep
            ["The",direction,"side","of","(/)","must","be","a"
            ,H.dullyellow "Float" <> ","
            ,"but","instead","I","am","seeing:"
            ]
        , []
        )
  )


badIDiv :: T.Localizer -> H.Doc -> T.Type -> T.Type -> (H.Doc, H.Doc)
badIDiv localizer direction tipe expected =
  (
    H.reflow $
      "The (//) operator is specifically for integer division:"
  ,
    if isFloat tipe then
      H.stack
        [ H.fillSep
            ["The",direction,"side","of","(//)","must","be","an"
            ,H.dullyellow "Int" <> ","
            ,"but","I","am","seeing","a",H.dullyellow "Float" <> "."
            ,"I","recommend","doing","the","conversion","explicitly"
            ,"with","one","of","these","functions:"
            ]
        , H.vcat
            [ H.green "round" <> " 3.5     == 4"
            , H.green "floor" <> " 3.5     == 3"
            , H.green "ceiling" <> " 3.5   == 4"
            , H.green "truncate" <> " 3.5  == 3"
            ]
        , H.link "Note" "Read" "implicit-casts" "to learn why Elm does not implicitly convert Ints to Floats."
        ]
    else
      loneType localizer tipe expected
        (
          H.fillSep
            ["The",direction,"side","of","(//)","must","be","an"
            ,H.dullyellow "Int" <> ","
            ,"but","instead","I","am","seeing:"
            ]
        , []
        )
  )



-- BAD BOOLS


badBool :: T.Localizer -> H.Doc -> H.Doc -> T.Type -> T.Type -> (H.Doc, H.Doc)
badBool localizer op direction tipe expected =
  (
    H.reflow $
      "I am struggling with this boolean operation:"
  ,
    loneType localizer tipe expected
      (
        H.fillSep
          ["Both","sides","of","(" <> op <> ")","must","be"
          ,H.dullyellow "Bool","values,","but","the",direction,"side","is:"
          ]
      ,
        []
      )
  )



-- BAD COMPARISON


badCompLeft :: T.Localizer -> Category -> String -> String -> T.Type -> T.Type -> (H.Doc, H.Doc)
badCompLeft localizer category op direction tipe expected =
  (
    H.reflow $
      "I cannot do a comparison with this value:"
  ,
    loneType localizer tipe expected
      (
        H.reflow $ toDescription category $
          "The " <> direction <> " side of (" <> op <> ") is"
      ,
        [ H.fillSep
            ["But","(" <> H.text op <> ")","only","works","on"
            ,H.dullyellow "Int" <> ","
            ,H.dullyellow "Float" <> ","
            ,H.dullyellow "Char" <> ","
            ,"and"
            ,H.dullyellow "String"
            ,"values.","It","can","work","on","lists","and","tuples"
            ,"of","comparable","values","as","well,","but","it","is"
            ,"usually","better","to","find","a","different","path."
            ]
        ]
      )
  )


badCompRight :: T.Localizer -> String -> T.Type -> T.Type -> RightDocs
badCompRight localizer op tipe expected =
  EmphBoth
    (
      H.reflow $
        "I need both sides of (" <> op <> ") to be the same type:"
    ,
      typeComparison localizer expected tipe
        (
          "The left side of is:"
        ,
          "But the right side is:"
        ,
          [ H.reflow $
              "I cannot compare different types though! Which side of (" <> op <> ") is the problem?"
          ]
        )
    )



-- BAD EQUALITY


badEquality :: T.Localizer -> String -> T.Type -> T.Type -> RightDocs
badEquality localizer op tipe expected =
  EmphBoth
    (
      H.reflow $
        "I need both sides of (" <> op <> ") to be the same type:"
    ,
      typeComparison localizer expected tipe
        (
          "The left side of is:"
        ,
          "But the right side is:"
        ,
          if isFloat tipe || isFloat expected then
            [ H.toSimpleNote $
                "Equality on floats is not 100% reliable due to the design of IEEE 754. I\
                \ recommend a check like (abs (x - y) < 0.0001) instead."
            ]
          else
            [ H.reflow $
                "Different types can never be equal though! Which side is messed up?"
            ]
        )
    )



-- INFINITE TYPES


toInfiniteReport :: Code.Source -> T.Localizer -> R.Region -> N.Name -> T.Type -> Report.Report
toInfiniteReport source localizer region name overallType =
  Report.Report "INFINITE TYPE" region [] $
    Report.toCodeSnippet source region Nothing
      (
        H.reflow $
          "I am inferring a weird self-referential type for " <> N.toString name <> ":"
      ,
        H.stack
          [ H.reflow $
              "Here is my best effort at writing down the type. You will see ∞ for\
              \ parts of the type that repeat something already printed out infinitely."
          , H.indent 4 (H.dullyellow (T.toDoc localizer RT.None overallType))
          , H.reflowLink
              "Staring at the type is usually not so helpful, so I recommend reading the hints at"
              "infinite-type"
              "to get unstuck."
          ]
      )
