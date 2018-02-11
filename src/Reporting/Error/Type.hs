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

import qualified AST.Module.Name as ModuleName
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
patternTypeComparison localizer actual expected ( iAmSeeing, insteadOf, soHereAreSomeThoughts ) =
  let
    (actualDoc, expectedDoc) =
      error "TODO diff" localizer actual expected
  in
  H.stack $
    [ H.reflow iAmSeeing
    , H.indent 4 actualDoc
    , H.reflow insteadOf
    , H.indent 4 expectedDoc
    ]
    ++ soHereAreSomeThoughts


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
typeComparison localizer actual expected ( iAmSeeing, insteadOf, soHereAreSomeThoughts ) =
  let
    (actualDoc, expectedDoc) =
      error "TODO diff" localizer actual expected
  in
  H.stack $
    [ H.reflow iAmSeeing
    , H.indent 4 actualDoc
    , H.reflow insteadOf
    , H.indent 4 expectedDoc
    ]
    ++ soHereAreSomeThoughts


indentType :: T.Localizer -> T.Type -> H.Doc
indentType localizer tipe =
  H.indent 4 (H.dullyellow (T.toDoc localizer RT.None tipe))


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
              [
              -- problem
              ]
            )
        )

    FromAnnotation name _arity subContext expectedType ->
      Report.toCodeSnippet source exprRegion Nothing
        (
          H.reflow $
            case subContext of
              TypedIfBranch index ->
                "Something is off with the " <> H.ordinalize (Index.toHuman index) <> " branch of this `if` experssion:"

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
              H.stack
                [ H.reflow $ toDescription category "It is"
                , indentType localizer tipe
                , H.fillSep
                    ["But","I","only","now","how","to","negate"
                    ,H.dullyellow "Int","and",H.dullyellow "Float","values."
                    ]
                --, problem
                ]
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
              H.stack
              [ H.reflow $ toDescription category "It is"
              , indentType localizer tipe
              , H.fillSep ["But","I","need","this","`if`","condition","to","be","a",H.dullyellow "Bool","value."]
              , H.toSimpleHint $
                  "Elm does not have “truthiness” such that ints and strings and lists\
                  \ are automatically converted to booleans. Do that conversion explicitly!"
              ]
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
                  H.stack $
                    [ H.reflow $
                        "It is a record with this type:"
                    , indentType localizer tipe
                    , H.reflow $
                        "So maybe the ." <> N.toString field <> " field access is misspelled?"
                    ]
                )

              _ ->
                (
                  H.reflow $
                    "This is not a record, so it has no fields to access!"
                ,
                  H.stack
                    [ H.reflow $ toDescription category "It is"
                    , indentType localizer tipe
                    , H.fillSep
                        ["But","I","need","a","record","with","a",H.dullyellow (H.nameToDoc field),"field!"]
                    ]
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
                  H.stack
                    [ H.reflow $ toDescription category "It is"
                    , indentType localizer tipe
                    , H.reflow $ "But I need some kind of record here!"
                    ]
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
opLeftToDocs localizer category op tipe expectedType =
  case op of
    "+"
      | isString tipe -> badStringAdd
      | isList tipe   -> badListAdd localizer category "left" tipe
      | otherwise     -> badMath localizer category "Addition" "left" "+" tipe []

    "*"
      | isList tipe  -> badListMul localizer category "left" tipe
      | otherwise    -> badMath localizer category "Multiplication" "left" "*" tipe []

    "-"  -> badMath localizer category "Subtraction" "left" "-" tipe []
    "^"  -> badMath localizer category "Exponentiation" "left" "^" tipe []
    "/"  -> badFDiv localizer "left" tipe
    "//" -> badIDiv localizer "left" tipe
    "&&" -> badBool localizer "&&" "left" tipe
    "||" -> badBool localizer "||" "left" tipe
    "<"  -> badCompLeft localizer category "<" "left" tipe
    ">"  -> badCompLeft localizer category ">" "left" tipe
    "<=" -> badCompLeft localizer category "<=" "left" tipe
    ">=" -> badCompLeft localizer category ">=" "left" tipe

    "<|" ->
      ( "The left side of (<|) needs to be a function so I can pipe arguments to it!"
      ,
        H.stack
          [ H.reflow $ toDescription category "I am seeing"
          , indentType localizer tipe
          , H.reflow $ "This needs to be some kind of function though!"
          ]
      )

    _ ->
      (
        H.reflow $
          "The left argument of (" <> N.toString op <> ") is causing problems:"
      ,
        typeComparison localizer tipe expectedType
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
opRightToDocs localizer category op tipe expectedType =
  case op of
    "+"
      | isInt tipe    -> badCast op FloatInt
      | isFloat tipe  -> badCast op IntFloat
      | isString tipe -> EmphRight $ badStringAdd
      | isList tipe   -> EmphRight $ badListAdd localizer category "right" tipe
      | otherwise     -> EmphRight $ badMath localizer category "Addition" "right" "+" tipe []

    "*"
      | isInt tipe   -> badCast op FloatInt
      | isFloat tipe -> badCast op IntFloat
      | isList tipe  -> EmphRight $ badListMul localizer category "right" tipe
      | otherwise    -> EmphRight $ badMath localizer category "Multiplication" "right" "*" tipe []

    "-"
      | isInt tipe   -> badCast op FloatInt
      | isFloat tipe -> badCast op IntFloat
      | otherwise    -> EmphRight $ badMath localizer category "Subtraction" "right" "-" tipe []

    "^"
      | isInt tipe   -> badCast op FloatInt
      | isFloat tipe -> badCast op IntFloat
      | otherwise    -> EmphRight $ badMath localizer category "Exponentiation" "right" "^" tipe []

    "/"  -> EmphRight $ badFDiv localizer "right" tipe
    "//" -> EmphRight $ badIDiv localizer "right" tipe
    "&&" -> EmphRight $ badBool localizer "&&" "right" tipe
    "||" -> EmphRight $ badBool localizer "||" "right" tipe
    "<"  -> badCompRight localizer "<" tipe expectedType
    ">"  -> badCompRight localizer ">" tipe expectedType
    "<=" -> badCompRight localizer "<=" tipe expectedType
    ">=" -> badCompRight localizer ">=" tipe expectedType
    "==" -> badEquality localizer "==" tipe expectedType
    "/=" -> badEquality localizer "/=" tipe expectedType

    "<|" ->
      EmphRight
        (
          H.reflow $
            "I cannot send this through the (<|) pipe:"
        ,
          typeComparison localizer tipe expectedType
            (
              toDescription category "You are providing"
            ,
              "But (<|) is piping it a function that expects:"
            ,
              []
            )
        )

    "|>" ->
      case (tipe, expectedType) of
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
              H.stack
                [ H.reflow $ toDescription category $
                    "Instead of a function, I am seeing "
                , indentType localizer tipe
                ]
            )

    _ ->
      EmphRight
        (
          H.reflow $
            "The right argument of (" <> N.toString op <> ") is causing problems."
        ,
          typeComparison localizer tipe expectedType
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
      home == ModuleName.basics && name == N.int

    _ ->
      False


isFloat :: T.Type -> Bool
isFloat tipe =
  case tipe of
    T.Type home name [] ->
      home == ModuleName.basics && name == N.float

    _ ->
      False


isString :: T.Type -> Bool
isString tipe =
  case tipe of
    T.Type home name [] ->
      home == ModuleName.string && name == N.string

    _ ->
      False


isList :: T.Type -> Bool
isList tipe =
  case tipe of
    T.Type home name [_] ->
      home == ModuleName.list && name == N.list

    _ ->
      False



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


badListAdd :: T.Localizer -> Category -> String -> T.Type -> (H.Doc, H.Doc)
badListAdd localizer category direction tipe =
  (
    "I cannot do addition with lists:"
  ,
    H.stack
      [ H.reflow $ toDescription category $
          "The " <> direction <> " side of (+) is"
      , indentType localizer tipe
      , H.fillSep
          ["But","(+)","only","works","with",H.dullyellow "Int","and",H.dullyellow "Float","values."
          ]
      , H.toFancyHint
          ["Switch","to","the",H.green "(++)","operator","to","append","lists!"
          ]
      ]
  )


badListMul :: T.Localizer -> Category -> String -> T.Type -> (H.Doc, H.Doc)
badListMul localizer category direction tipe =
  badMath localizer category "Multiplication" direction "*" tipe
    [
      H.toFancyHint
        [ "Maybe", "you", "want"
        , H.green "List.repeat"
        , "to", "build","a","list","of","repeated","values?"
        ]
    ]


badMath :: T.Localizer -> Category -> String -> String -> String -> T.Type -> [H.Doc] -> (H.Doc, H.Doc)
badMath localizer category operation direction op tipe otherHints =
  (
    H.reflow $
      operation ++ " does not work with this value:"
  ,
    H.stack $
      [ H.reflow $ toDescription category $
          "The " <> direction <> " side of (" <> op <> ") is"
      , indentType localizer tipe
      , H.fillSep
          ["But","(" <> H.text op <> ")","only","works","with"
          ,H.dullyellow "Int","and",H.dullyellow "Float","values."
          ]
      ]
      ++ otherHints
  )


badFDiv :: T.Localizer -> H.Doc -> T.Type -> (H.Doc, H.Doc)
badFDiv localizer direction tipe =
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
      H.stack
        [ H.fillSep
            ["The",direction,"side","of","(/)","must","be","a"
            ,H.dullyellow "Float" <> ","
            ,"but","instead","I","am","seeing:"
            ]
        , indentType localizer tipe
        ]
  )


badIDiv :: T.Localizer -> H.Doc -> T.Type -> (H.Doc, H.Doc)
badIDiv localizer direction tipe =
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
      H.stack
        [ H.fillSep
            ["The",direction,"side","of","(//)","must","be","an"
            ,H.dullyellow "Int" <> ","
            ,"but","instead","I","am","seeing:"
            ]
        , indentType localizer tipe
        ]
  )



-- BAD BOOLS


badBool :: T.Localizer -> H.Doc -> H.Doc -> T.Type -> (H.Doc, H.Doc)
badBool localizer op direction tipe =
  (
    H.reflow $
      "I am struggling with this boolean operation:"
  ,
    H.stack
      [ H.fillSep
          ["Both","sides","of","(" <> op <> ")","must","be"
          ,H.dullyellow "Bool","values,","but","the",direction,"side","is:"
          ]
      , indentType localizer tipe
      , H.toSimpleHint
          "Elm does not have “truthiness” such that ints and strings and lists are\
          \ automatically converted to booleans. Do that conversion explicitly!"
      ]
  )



-- BAD COMPARISON


badCompLeft :: T.Localizer -> Category -> String -> String -> T.Type -> (H.Doc, H.Doc)
badCompLeft localizer category op direction tipe =
  (
    H.reflow $
      "I cannot do a comparison with this value:"
  ,
    H.stack
      [ H.reflow $ toDescription category $
          "The " <> direction <> " side of (" <> op <> ") is"
      , indentType localizer tipe
      , H.fillSep
          ["But","(" <> H.text op <> ")","only","works","on"
          ,H.dullyellow "Int" <> ","
          ,H.dullyellow "Float" <> ","
          ,H.dullyellow "Char" <> ","
          ,"and"
          ,H.dullyellow "String"
          ,"values.","If","you","want","to","get","crazy,","it","will","work","on"
          ,"lists","and","tuples","of","comparable","values","as","well."
          ]
      ]
  )


badCompRight :: T.Localizer -> String -> T.Type -> T.Type -> RightDocs
badCompRight localizer op tipe expectedType =
  EmphBoth
    (
      H.reflow $
        "I need both sides of (" <> op <> ") to be the same type:"
    ,
      typeComparison localizer expectedType tipe
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
badEquality localizer op tipe expectedType =
  EmphBoth
    (
      H.reflow $
        "I need both sides of (" <> op <> ") to be the same type:"
    ,
      typeComparison localizer expectedType tipe
        (
          "The left side of is:"
        ,
          "But the right side is:"
        ,
          if isFloat tipe || isFloat expectedType then
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
          , indentType localizer overallType
          , H.reflowLink
              "Staring at the type is usually not so helpful, so I recommend reading the hints at"
              "infinite-type"
              "to get unstuck."
          ]
      )
