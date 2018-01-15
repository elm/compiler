{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Reporting.Error.Type
  ( Error(..)
  , Origin(..)
  , toReport
  )
  where


import Prelude hiding (round)
import Data.Monoid ((<>))

import qualified AST.Canonical as Can
import qualified AST.Utils.Type as Type
import qualified Data.Index as Index
import qualified Elm.Name as N
import qualified Reporting.Helpers as H
import qualified Reporting.Region as R
import qualified Reporting.Render.Code as Code
import qualified Reporting.Render.Type as Type
import qualified Reporting.Report as Report
import qualified Type.Constraint as C



-- ERRORS


data Error
  = Mismatch Origin (C.Expected Can.Type)
  | BadPattern R.Region C.PCategory Can.Type (C.PExpected Can.Type)
  | InfiniteType Can.Type


data Origin
  = BadExpr R.Region C.Category Can.Type
  | BadLocal R.Region N.Name Can.Type
  | BadForeign R.Region N.Name Can.Type



-- TO REPORT


toReport :: Code.Source -> Type.Localizer -> Error -> Report.Report
toReport source localizer err =
  case err of
    Mismatch origin expected ->
      case origin of
        BadForeign region name tipe ->
          toMismatchReport source localizer region (Var name) tipe expected

        BadLocal region name tipe ->
          toMismatchReport source localizer region (Var name) tipe expected

        BadExpr region category tipe ->
          toMismatchReport source localizer region (Category category) tipe expected

    BadPattern region category tipe expected ->
      error "TODO" region category tipe expected

    InfiniteType overallType ->
      error "TODO" source localizer overallType



-- HELPERS


data Thing
  = Var N.Name
  | Category C.Category


typeComparison :: Type.Localizer -> Can.Type -> Can.Type -> ( String, String, [H.Doc] ) -> H.Doc
typeComparison localizer actual expected ( iAmSeeing, insteadOf, soHereAreSomeThoughts ) =
  H.stack $
    [ H.reflow iAmSeeing
    , H.indent 4 (Type.toDoc localizer actual)
    , H.reflow insteadOf
    , H.indent 4 (Type.toDoc localizer expected)
    ]
    ++ soHereAreSomeThoughts


toDescription :: Thing -> String -> String
toDescription thing thatThingIs =
  case thing of
    Var name ->
      "This `" <> N.toString name <> "` value has type:"

    Category category ->
      case category of
        C.List -> thatThingIs <> " a list of type:"
        C.Number -> thatThingIs <> " a number of type:"
        C.Float -> thatThingIs <> " a float of type:"
        C.String -> thatThingIs <> " a string of type:"
        C.Char -> thatThingIs <> " a character of type:"
        C.If -> "This `if` expression produces a value of type:"
        C.Case -> "This `case` expression produces a value of type:"
        C.Lambda -> thatThingIs <> " an anonymous function of type:"
        C.Accessor field -> "This ." <> N.toString field <> " field access function has type of type:"
        C.Access field -> "The value at ." <> N.toString field <> " has type of type:"
        C.Record -> thatThingIs <> " a record of type:"
        C.Tuple -> thatThingIs <> " a tuple of type:"
        C.Unit -> thatThingIs <> " a unit value:"
        C.Shader -> thatThingIs <> " a GLSL shader of type:"
        C.Effects -> thatThingIs <> " a thing for CORE LIBRARIES ONLY."
        C.CallResult maybeName ->
          case maybeName of
            C.NoName -> thatThingIs <> ":"
            C.FuncName name -> "This `" <> N.toString name <> "` call produces:"
            C.CtorName name -> "This `" <> N.toString name <> "` call produces:"
            C.OpName _ -> thatThingIs <> ":"



-- TO MISMATCH REPORT


toMismatchReport :: Code.Source -> Type.Localizer -> R.Region -> Thing -> Can.Type -> C.Expected Can.Type -> Report.Report
toMismatchReport source localizer exprRegion thing tipe expected =
  Report.Report "TYPE MISMATCH" exprRegion [] $
  case expected of
    C.NoExpectation expectedType ->
      Report.toCodeSnippet source exprRegion Nothing
        (
          "This expression is being used in an unexpected way:"
        ,
          typeComparison localizer tipe expectedType
            (
              toDescription thing "It is"
            ,
              "But you are trying to use it as:"
            ,
              [
              -- problem
              ]
            )
        )

    C.FromAnnotation name arity subContext expectedType ->
      Report.toCodeSnippet source exprRegion Nothing
        (
          H.reflow $
            case subContext of
              C.TypedIfBranch index ->
                "Something is off with the " <> H.ordinalize (Index.toHuman index) <> " branch of this `if` experssion:"

              C.TypedCaseBranch index ->
                "Something is off with the " <> H.ordinalize (Index.toHuman index) <> " branch of this `case` expression:"

              C.TypedBody ->
                "Something is off with the body of the `" <> N.toString name <> "` definition:"
        ,
          typeComparison localizer tipe expectedType
            (
              toDescription thing $
                case subContext of
                  C.TypedIfBranch index ->
                    "The " <> H.ordinalize (Index.toHuman index) <> " branch is"

                  C.TypedCaseBranch index ->
                    "The " <> H.ordinalize (Index.toHuman index) <> " branch is"

                  C.TypedBody ->
                    "The body is"
            ,
              "But the type annotation on `" <> N.toString name <> "` says it should be:"
            ,
              [ error "TODO check for arity issues. Suggest that an arg is unlisted." arity
              --, problem
              ]
            )
        )

    C.FromContext region context expectedType ->
      case context of
        C.ListEntry index ->
          if Index.toHuman index == 2 then
            error "TODO 1st vs 2nd list entry"
          else
            Report.toCodeSnippet source region (Just exprRegion)
              (
                H.reflow $
                  "The " <> H.ordinalize (Index.toHuman index) <> " element of this list does not match all the previous elements:"
              ,
                typeComparison localizer tipe expectedType
                  (
                    toDescription thing $
                      "The " <> H.ordinalize (Index.toHuman index) <> " element is"
                  ,
                    "But all the previous elements in the list are:"
                  ,
                    [ H.toSimpleHint $
                        "Everything in the list needs to be the same type of value.\
                        \ This way you never run into unexpected values partway through.\
                        \ To mix different types in a single list, create a \"union type\" as\
                        \ described in: <http://guide.elm-lang.org/types/union_types.html>"
                    --, problem
                    ]
                  )
              )

        C.Negate ->
          Report.toCodeSnippet source region (Just exprRegion)
            (
              "I do not know how to negate this type of value:"
            ,
              H.stack
                [ H.reflow $ toDescription thing "It is"
                , Type.toDoc localizer tipe
                , H.fillSep
                    ["But","I","only","now","how","to","negate"
                    ,H.dullyellow "Int","and",H.dullyellow "Float","values."
                    ]
                --, problem
                ]
            )

        C.OpLeft op ->
          Report.toCodeSnippet source region (Just exprRegion) $
            opLeftToDocs localizer thing op tipe expectedType

        C.OpRight op ->
          Report.toCodeSnippet source region (Just exprRegion) $
            opRightToDocs localizer thing op tipe expectedType

        C.IfCondition ->
          Report.toCodeSnippet source region (Just exprRegion)
            (
              "This `if` condition does not evaluate to a boolean value, True or False."
            ,
              H.stack
              [ H.reflow $ toDescription thing "It is"
              , Type.toDoc localizer tipe
              , H.fillSep ["But","I","need","this","`if`","condition","to","be","a",H.dullyellow "Bool","value."]
              , H.toSimpleHint $
                  "Elm does not have “truthiness” such that ints and strings and lists\
                  \ are automatically converted to booleans. Do that conversion explicitly!"
              ]
            )

        C.IfBranch index ->
          if Index.toHuman index == 2 then
            error "TODO 1st vs 2nd if branches"
            -- error "TODO if with no chaining"
          else
            Report.toCodeSnippet source region (Just exprRegion)
              (
                H.reflow $
                  "The " <> H.ordinalize (Index.toHuman index) <> " branch of this `if` does not match all the previous branches:"
              ,
                typeComparison localizer tipe expectedType
                  (
                    toDescription thing $
                      "The " <> H.ordinalize (Index.toHuman index) <> " branch is"
                  ,
                    "But all the previous branches result in:"
                  ,
                    [ H.link "Hint"
                        "All branches in an `if` must produce the same type of values. This way, no\
                        \ matter which branch we take, the result is always a consistent shape. Read"
                        "union-types"
                        "to learn how to “mix” types."
                    --, problem
                    ]
                  )
              )

        C.CaseBranch index ->
          if Index.toHuman index == 2 then
            error "TODO 1st vs 2nd case branches"
          else
            Report.toCodeSnippet source region (Just exprRegion)
              (
                H.reflow $
                  "The " <> H.ordinalize (Index.toHuman index) <> " branch of this `case` does not match all the previous branches:"
              ,
                typeComparison localizer tipe expectedType
                  (
                    toDescription thing $
                      "The " <> H.ordinalize (Index.toHuman index) <> " branch is"
                  ,
                    "But all the previous branches result in:"
                  ,
                    [ H.link "Hint"
                        "All branches in a `case` must produce the same type of values. This way, no\
                        \ matter which branch we take, the result is always a consistent shape. Read"
                        "union-types"
                        "to learn how to “mix” types."
                    --, problem
                    ]
                  )
              )

        C.CallArg maybeFuncName index ->
          let
            thisFunction =
              case maybeFuncName of
                C.NoName ->
                  "this function"

                C.FuncName name ->
                  "`" <> N.toString name <> "`"

                C.CtorName name ->
                  "`" <> N.toString name <> "`"

                C.OpName op ->
                  "(" <> N.toString op <> ")"

          in
          Report.toCodeSnippet source region (Just exprRegion)
            (
              H.reflow $
                "The " <> H.ordinalize (Index.toHuman index) <> " argument to " <> thisFunction <> " is not what I expect:"
            ,
              typeComparison localizer tipe expectedType
                (
                  toDescription thing "This argument is"
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

        C.TooManyArgs maybeName numGivenArgs ->
          case length (Type.delambda tipe) - 1 of
            0 ->
              Report.toCodeSnippet source region (Just exprRegion)
                (
                  H.reflow $
                    case maybeName of
                      C.NoName ->
                        "You are giving " <> H.args numGivenArgs <> " to something that is not a function!"

                      C.FuncName name ->
                        "`" <> N.toString name <> "` is not a function, but you are giving it " <> H.args numGivenArgs <> "!"

                      C.CtorName name ->
                        "`" <> N.toString name <> "` is not a function, but you are giving it " <> H.args numGivenArgs <> "!"

                      C.OpName op ->
                        "(" <> N.toString op <> ") is not a function, but you are giving it " <> H.args numGivenArgs <> "!"
                ,
                  typeComparison localizer tipe expectedType
                    (
                      case maybeName of
                        C.NoName ->
                          "It is a value of type:"

                        C.FuncName name ->
                          "`" <> N.toString name <> "` is a value of type:"

                        C.CtorName name ->
                          "`" <> N.toString name <> "` is a constructor of type:"

                        C.OpName op ->
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
                    C.NoName ->
                      "This function"

                    C.FuncName name ->
                      "The `" <> N.toString name <> "` function"

                    C.CtorName name ->
                      "The `" <> N.toString name <> "` constructor"

                    C.OpName op ->
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

        C.RecordAccess field ->
          Report.toCodeSnippet source region (Just exprRegion) $
            case Type.deepDealias tipe of
              Can.TRecord _ _ ->
                (
                  H.reflow $
                    "This record does not have a `" <> N.toString field <> "` field:"
                ,
                  H.stack $
                    [ H.reflow $
                        "It is a record with this type:"
                    , Type.toDoc localizer tipe
                    , H.reflow $
                        "So maybe the ." <> N.toString field <> " field access is misspelled?"
                    ]
                    ++
                      case error "TODO closeNames" of
                        [] ->
                          []

                        [name] ->
                          [ H.toFancyHint $
                              ["It","may","also","be","a","typo","in","the","record","itself."
                              ,"Perhaps","the",H.dullyellow name,"field","is","misspelled?"
                              ]
                          ]

                        names ->
                          [ H.toFancyHint $
                              ["It","may","also","be","a","typo","in","the","record","itself."
                              ,"Perhaps","the"
                              ]
                              ++ H.commaSep "or" H.dullyellow names
                              ++ ["field","is","misspelled?"]
                          ]
                )

              Can.TLambda _ _ ->
                (
                  H.reflow $
                    "I cannot access field `" <> N.toString field <> "` of this " <> whatever <> ":"
                ,
                  H.stack
                    [ H.reflow $
                        "The " <> whatever <> " is not a record! It is actually a function:"
                    , Type.toDoc localizer tipe
                    , H.toSimpleHint "Maybe it is missing some arguments?"
                    ]
                )

              _ ->
                (
                  H.reflow $
                    "I cannot access field `" <> N.toString field <> "` of this " <> whatever <> ":"
                ,
                  H.stack
                    [ H.reflow $
                        "The " <> whatever <> " is not a record though! It is a:"
                    , Type.toDoc localizer tipe
                    ]
                )

        C.RecordUpdate ->
          case Type.deepDealias tipe of
            Can.TRecord _ _ ->
              Report.toCodeSnippet source region Nothing
                (
                  H.reflow $
                    "I think there is a typo in one of these field names:"
                ,
                  error "TODO record update field name typo"
                )

            _ ->
              Report.toCodeSnippet source region (Just exprRegion)
                (
                  "The record update syntax does not work with this value:"
                ,
                  H.stack
                    [ H.reflow $
                        "I can only update records, but this appears to be a " <> whatever <> ":"
                    , Type.toDoc localizer tipe
                    ]
                )

        C.Destructure ->
          error "TODO Destructure"



-- OP LEFT


opLeftToDocs :: Type.Localizer -> Thing -> N.Name -> Can.Type -> Can.Type -> (H.Doc, H.Doc)
opLeftToDocs localizer thing op tipe expectedType =
  case op of
    "+"
      | isString tipe -> badStringAdd
      | isList tipe   -> badListAdd localizer "left" tipe
      | otherwise     -> badMath localizer "Addition" "left" "+" tipe []

    "*"
      | isList tipe  -> badListMul localizer "left" tipe
      | otherwise    -> badMath localizer "Multiplication" "left" "*" tipe []

    "-"  -> badMath localizer "Subtraction" "left" "-" tipe []
    "^"  -> badMath localizer "Exponentiation" "left" "^" tipe []
    "/"  -> badFDiv localizer "left" tipe
    "//" -> badIDiv localizer "left" tipe
    "&&" -> badBool localizer "&&" "left" tipe
    "||" -> badBool localizer "||" "left" tipe
    "<"  -> badCompArg localizer thing "<" "left" tipe
    ">"  -> badCompArg localizer thing ">" "left" tipe
    "<=" -> badCompArg localizer thing "<=" "left" tipe
    ">=" -> badCompArg localizer thing ">=" "left" tipe

    "<|" ->
      ( "The left side of (<|) needs to be a function so I can pipe arguments to it!"
      ,
        H.stack
          [ H.reflow $
              "Instead of a function, I am seeing " <> whatever <> ":"
          , Type.toDoc localizer tipe
          , error "TODO"
            {-
            case category of
              C.Call ->
                "Maybe you gave " <> funcName <> " too many arguments already?"

              _ ->
                []
            -}
          ]
      )

    _ ->
      (
        H.reflow $
          "The left argument of (" <> N.toString op <> ") is causing problems:"
      ,
        typeComparison localizer tipe expectedType
          (
            toDescription thing "The left argument is"
          ,
            "But (" <> N.toString op <> ") needs the left argument to be:"
          ,
            []
          )
      )



-- OP RIGHT


opRightToDocs :: Type.Localizer -> Thing -> N.Name -> Can.Type -> Can.Type -> (H.Doc, H.Doc)
opRightToDocs localizer thing op tipe expectedType =
  case op of
    "+"
      | isInt tipe    -> badCast op FloatInt
      | isFloat tipe  -> badCast op IntFloat
      | isString tipe -> badStringAdd
      | isList tipe   -> badListAdd localizer "right" tipe
      | otherwise     -> badMath localizer "Addition" "right" "+" tipe []

    "*"
      | isInt tipe   -> badCast op FloatInt
      | isFloat tipe -> badCast op IntFloat
      | isList tipe  -> badListMul localizer "right" tipe
      | otherwise    -> badMath localizer "Multiplication" "right" "*" tipe []

    "-"
      | isInt tipe   -> badCast op FloatInt
      | isFloat tipe -> badCast op IntFloat
      | otherwise    -> badMath localizer "Subtraction" "right" "-" tipe []

    "^"
      | isInt tipe   -> badCast op FloatInt
      | isFloat tipe -> badCast op IntFloat
      | otherwise    -> badMath localizer "Exponentiation" "right" "^" tipe []

    "/"  -> badFDiv localizer "right" tipe
    "//" -> badIDiv localizer "right" tipe
    "&&" -> badBool localizer "&&" "right" tipe
    "||" -> badBool localizer "||" "right" tipe
    "<"  -> badCompRight localizer thing "<" tipe expectedType
    ">"  -> badCompRight localizer thing ">" tipe expectedType
    "<=" -> badCompRight localizer thing "<=" tipe expectedType
    ">=" -> badCompRight localizer thing ">=" tipe expectedType
    "==" -> badEquality localizer "==" tipe expectedType
    "/=" -> badEquality localizer "/=" tipe expectedType

    "<|" ->
        (
          H.reflow $
            "I cannot pipe this " <> whatever <> " in as the final argument:"
        ,
          typeComparison localizer tipe expectedType
            (
              toDescription thing "You are providing"
            ,
              "But (<|) is piping it a function that expects:"
            ,
              [ "Maybe an argument is missing?"
--              , problem
              ]
            )
        )

    "|>" ->
      case (tipe, expectedType) of
        (Can.TLambda expectedArgType _, Can.TLambda argType _) ->
            (
              "I cannot pipe the argument through to this function:"
            ,
              typeComparison localizer argType expectedArgType
                (
                  "You are providing an argument of type:"
                ,
                  "But (|>) is piping it a function that expects its next argument to be:"
                  {-
                  case maybeName of
                    Nothing ->
                      "But (|>) is piping it a function that expects the next argument to be:"

                    Just (C.FuncName name) ->
                      "But (|>) is piping it to `" <> N.toString name <> "` which expects the next argument to be:"

                    Just (C.OpName op) ->
                      "But (|>) is piping it to (" <> N.toString op <> ") which expects the next argument to be:"
                  -}
                ,
                  [ "Maybe an argument is missing?"
--                  , problem
                  ]
                )
            )

        _ ->
            (
              "The right side of (|>) needs to be a function so I can pipe arguments to it!"
            ,
              H.stack
                [ H.reflow $
                    "Instead of a function, I am seeing " <> whatever <> ":"
                , Type.toDoc localizer tipe
                , error "TODO"
                  {-
                  case category of
                    Call ->
                      "Maybe you gave " <> funcName <> " too many arguments already?"

                    _ ->
                      []
                  -}
                ]
            )

    _ ->
        (
          H.reflow $
            "The right argument of (" <> N.toString op <> ") is causing problems."
        ,
          typeComparison localizer tipe expectedType
            (
              toDescription thing "The right argument is"
            ,
              "But (" <> N.toString op <> ") needs the right argument to be:"
            ,
              error "TODO"
              {-
              [ problem
              , binopHint home op leftType rightType
              ]
              -}
            )
        )



isInt :: Can.Type -> Bool
isInt tipe =
  error "TODO isInt" tipe


isFloat :: Can.Type -> Bool
isFloat tipe =
  error "TODO isFloat" tipe


isString :: Can.Type -> Bool
isString tipe =
  error "TODO isString" tipe


isList :: Can.Type -> Bool
isList tipe =
  error "TODO isList" tipe


whatever :: String
whatever =
  error "TODO whatever"



-- BAD MATH


data ThisThenThat = FloatInt | IntFloat


badCast :: N.Name -> ThisThenThat -> (H.Doc, H.Doc)
badCast op thisThenThat =
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


badListAdd :: Type.Localizer -> String -> Can.Type -> (H.Doc, H.Doc)
badListAdd localizer direction tipe =
  (
    "I cannot do addition with lists:"
  ,
    H.stack
      [ H.reflow $
          "The " <> direction <> " side of (+) is " <> whatever <> ":"
      , Type.toDoc localizer tipe
      , H.fillSep
          ["But","(+)","only","works","with",H.dullyellow "Int","and",H.dullyellow "Float","values."
          ]
      , H.toFancyHint
          ["Switch","to","the",H.green "(++)","operator","to","append","lists!"
          ]
      ]
  )


badListMul :: Type.Localizer -> String -> Can.Type -> (H.Doc, H.Doc)
badListMul localizer direction tipe =
  badMath localizer "Multiplication" direction "*" tipe
    [
      H.toFancyHint
        [ "Maybe", "you", "want"
        , H.green "List.repeat"
        , "to", "build","a","list","of","repeated","values?"
        ]
    ]


badMath :: Type.Localizer -> String -> String -> String -> Can.Type -> [H.Doc] -> (H.Doc, H.Doc)
badMath localizer operation direction op tipe otherHints =
  (
    H.reflow $
      operation ++ " does not work with this value:"
  ,
    H.stack $
      [ H.reflow $
          "The " <> direction <> " side of (" <> op <> ") is a " <> whatever <> ":"
      , Type.toDoc localizer tipe
      , H.fillSep
          ["But","(" <> H.text op <> ")","only","works","with"
          ,H.dullyellow "Int","and",H.dullyellow "Float","values."
          ]
      , error "TODO check for missing arguments"
      ]
      ++ otherHints
  )


badFDiv :: Type.Localizer -> H.Doc -> Can.Type -> (H.Doc, H.Doc)
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
        , Type.toDoc localizer tipe
        ]
  )


badIDiv :: Type.Localizer -> H.Doc -> Can.Type -> (H.Doc, H.Doc)
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
        , Type.toDoc localizer tipe
        ]
  )



-- BAD BOOLS


badBool :: Type.Localizer -> H.Doc -> H.Doc -> Can.Type -> (H.Doc, H.Doc)
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
      , Type.toDoc localizer tipe
      , H.toSimpleHint
          "Elm does not have “truthiness” such that ints and strings and lists are\
          \ automatically converted to booleans. Do that conversion explicitly!"
      ]
  )



-- BAD COMPARISON


badCompRight :: Type.Localizer -> Thing -> String -> Can.Type -> Can.Type -> (H.Doc, H.Doc)
badCompRight localizer thing op tipe expectedType =
  case tipe of
    Can.TVar name | N.startsWith "comparable" name ->
      badCompArg localizer thing op "right" tipe

    _ ->
      badCompMix localizer op tipe expectedType


badCompArg :: Type.Localizer -> Thing -> String -> String -> Can.Type -> (H.Doc, H.Doc)
badCompArg localizer thing op direction tipe =
  (
    H.reflow $
      "I cannot do a comparison with this value:"
  ,
    H.stack
      [ H.reflow $ toDescription thing $
          "The " <> direction <> " side of (" <> op <> ") is"
      , Type.toDoc localizer tipe
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


badCompMix :: Type.Localizer -> String -> Can.Type -> Can.Type -> (H.Doc, H.Doc)
badCompMix localizer op tipe expectedType =
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


badEquality :: Type.Localizer -> String -> Can.Type -> Can.Type -> (H.Doc, H.Doc)
badEquality localizer op tipe expectedType =
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
--        else
--          error "TODO detect problems here instead?"
      )
  )
