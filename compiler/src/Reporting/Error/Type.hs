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
import qualified Data.NonEmptyMap as NEM

import qualified AST.Canonical as Can
import qualified AST.Prim.Fields as F
import qualified AST.Prim.Name as N
import qualified AST.Prim.Operator as Op
import qualified AST.Prim.TypeName as T
import qualified AST.Prim.TypeVar as T (Var(..), Constraint(..), varToChars)
import qualified Elm.ModuleName as ModuleName
import qualified Data.Index as Index
import qualified Reporting.Annotation as A
import qualified Reporting.Doc as D
import qualified Reporting.Render.Code as Code
import qualified Reporting.Render.Type as T
import qualified Reporting.Report as Report
import qualified Reporting.Suggest as Suggest



-- ERRORS


data Error
  = BadExpr A.Region Category T.Type (Expected T.Type)
  | BadPattern A.Region PCategory T.Type (PExpected T.Type)
  | InfiniteType A.Region N.Name T.Type



-- EXPRESSION EXPECTATIONS


data Expected tipe
  = NoExpectation tipe
  | FromContext A.Region Context tipe
  | FromAnnotation N.Name Int SubContext tipe


data Context
  = ListEntry Index.ZeroBased
  | Negate
  | OpLeft Op.Name
  | OpRight Op.Name
  | IfCondition
  | IfBranch Index.ZeroBased
  | CaseBranch Index.ZeroBased
  | CallArity MaybeName Int
  | CallArg MaybeName Index.ZeroBased
  | RecordAccess A.Region (Maybe N.Name) A.Region N.Name
  | RecordUpdateKeys N.Name (NEM.Map N.Name Can.Update)
  | RecordUpdateValue N.Name
  | Destructure


data SubContext
  = TypedIfBranch Index.ZeroBased
  | TypedCaseBranch Index.ZeroBased
  | TypedBody


data MaybeName
  = FuncName N.Name
  | CtorName N.Name
  | RcrdName N.Name
  | OpName Op.Name
  | NoName


data Category
  = List
  | Bool
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
  | Operator Op.Name



-- PATTERN EXPECTATIONS


data PExpected tipe
  = PNoExpectation tipe
  | PFromContext A.Region PContext tipe


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


toPatternReport :: Code.Source -> T.Localizer -> A.Region -> PCategory -> T.Type -> PExpected T.Type -> Report.Report
toPatternReport source localizer patternRegion category tipe expected =
  Report.Report "TYPE MISMATCH" patternRegion [] $
  case expected of
    PNoExpectation expectedType ->
      Code.toSnippet source patternRegion Nothing $
        ( "This pattern is being used in an unexpected way:"
        , patternTypeComparison localizer tipe expectedType
            (addPatternCategory "It is" category)
            "But it needs to match:"
            []
        )

    PFromContext region context expectedType ->
      Code.toSnippet source region (Just patternRegion) $
        case context of
          PTypedArg name index ->
            ( D.reflow $
                "The " <> D.ordinal index <> " argument to `" <> N.toChars name <> "` is weird."
            , patternTypeComparison localizer tipe expectedType
                (addPatternCategory "The argument is a pattern that matches" category)
                ( "But the type annotation on `" <> N.toChars name
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
                      "custom-types"
                      "to handle “mixing” types."
                  ]
              )

          PCtorArg name index ->
            ( D.reflow $
                "The " <> D.ordinal index <> " argument to `" <> N.toChars name <> "` is weird."
            , patternTypeComparison localizer tipe expectedType
                (addPatternCategory "It is trying to match" category)
                ( "But `" <> N.toChars name <> "` needs its "
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
                [ D.link "Hint"
                    "Everything in a list must be the same type of value. This way, we never\
                    \ run into unexpected values partway through a List.map, List.foldl, etc. Read"
                    "custom-types"
                    "to learn how to “mix” types."
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


patternTypeComparison :: T.Localizer -> T.Type -> T.Type -> String -> String -> [D.Doc] -> D.Doc
patternTypeComparison localizer actual expected iAmSeeing insteadOf contextHints =
  let
    (actualDoc, expectedDoc, problems) =
      T.toDiffDocs localizer actual expected
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
      PCtor name -> " `" <> N.toChars name <> "` values of type:"
      PInt -> " integers:"
      PStr -> " strings:"
      PChr -> " characters:"
      PBool -> " booleans:"



-- EXPR HELPERS


typeComparison :: T.Localizer -> T.Type -> T.Type -> String -> String -> [D.Doc] -> D.Doc
typeComparison localizer actual expected iAmSeeing insteadOf contextHints =
  let
    (actualDoc, expectedDoc, problems) =
      T.toDiffDocs localizer actual expected
  in
  D.stack $
    [ D.reflow iAmSeeing
    , D.indent 4 actualDoc
    , D.reflow insteadOf
    , D.indent 4 expectedDoc
    ]
    ++ contextHints
    ++ problemsToHint problems


loneType :: T.Localizer -> T.Type -> T.Type -> D.Doc -> [D.Doc] -> D.Doc
loneType localizer actual expected iAmSeeing furtherDetails =
  let
    (actualDoc, _, problems) =
      T.toDiffDocs localizer actual expected
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
    Local n      -> "This `" <> N.toChars n <> "` value is a:"
    Foreign n    -> "This `" <> N.toChars n <> "` value is a:"
    Operator op  -> "This (" <> Op.toChars op <> ") operator is a:"
    Access f     -> "The value at ." <> N.toChars f <> " is a:"
    Accessor f   -> "This ." <> N.toChars f <> " field access function has type:"
    If           -> "This `if` expression produces:"
    Case         -> "This `case` expression produces:"
    List         -> thisIs <> " a list of type:"
    Bool         -> thisIs <> " a boolean of type:"
    Number       -> thisIs <> " a number of type:"
    Float        -> thisIs <> " a float of type:"
    String       -> thisIs <> " a string of type:"
    Char         -> thisIs <> " a character of type:"
    Lambda       -> thisIs <> " an anonymous function of type:"
    Record       -> thisIs <> " a record of type:"
    Tuple        -> thisIs <> " a tuple of type:"
    Unit         -> thisIs <> " a unit value:"
    Shader       -> thisIs <> " a GLSL shader of type:"
    Effects      -> thisIs <> " a thing for CORE LIBRARIES ONLY."
    CallResult m ->
      case m of
        NoName     -> thisIs <> ":"
        FuncName n -> "This `" <> N.toChars n <> "` call produces:"
        CtorName n -> "This `" <> N.toChars n <> "` call produces:"
        RcrdName n -> "This `" <> N.toChars n <> "` call produces:"
        OpName   _ -> thisIs <> ":"



--------------------------------------------------------------------------------
-- PROBLEM HINTS ---------------------------------------------------------------
--------------------------------------------------------------------------------


problemsToHint :: [T.Problem] -> [D.Doc]
problemsToHint =
    go
  where
    go problems =
      case problems of
        [] ->
          []

        p:ps ->
          case problemToHint p of
            Hint0      -> go ps
            Hint1 d    -> [d]
            HintN d ds -> d:ds


data Hint
  = Hint0
  | Hint1 D.Doc
  | HintN D.Doc [D.Doc]


problemToHint :: T.Problem -> Hint
problemToHint problem =
  case problem of
    T.IntFloat ->
      Hint1 $
        D.fancyLink "Note" ["Read"] "implicit-casts"
          ["to","learn","why","Elm","does","not","implicitly","convert"
          ,"Ints","to","Floats.","Use",D.green "toFloat","and"
          ,D.green "round","to","do","explicit","conversions."
          ]

    T.StringFromInt ->
      Hint1 $
        D.toFancyHint
          ["Want","to","convert","an","Int","into","a","String?"
          ,"Use","the",D.green "String.fromInt","function!"
          ]

    T.StringFromFloat ->
      Hint1 $
        D.toFancyHint
          ["Want","to","convert","a","Float","into","a","String?"
          ,"Use","the",D.green "String.fromFloat","function!"
          ]

    T.StringToInt ->
      Hint1 $
        D.toFancyHint
          ["Want","to","convert","a","String","into","an","Int?"
          ,"Use","the",D.green "String.toInt","function!"
          ]

    T.StringToFloat ->
      Hint1 $
        D.toFancyHint
          ["Want","to","convert","a","String","into","a","Float?"
          ,"Use","the",D.green "String.toFloat","function!"
          ]

    T.AnythingToBool ->
      Hint1 $
        D.toSimpleHint $
          "Elm does not have “truthiness” such that ints and strings and lists\
          \ are automatically converted to booleans. Do that conversion explicitly!"

    T.AnythingFromMaybe ->
      Hint1 $
        D.toFancyHint
          ["Use",D.green "Maybe.withDefault","to","handle","possible","errors."
          ,"Longer","term,","it","is","usually","better","to","write","out","the"
          ,"full","`case`","though!"
          ]

    T.ArityMismatch x y ->
      Hint1 $
        D.toSimpleHint $
          if x < y
          then "It looks like it takes too few arguments. I was expecting " ++ show (y - x) ++ " more."
          else "It looks like it takes too many arguments. I see " ++ show (x - y) ++ " extra."

    T.HaveFlex x t -> badFlexHint Have x t
    T.WantFlex t x -> badFlexHint Want x t

    T.HaveRigid x t -> badRigidHint x t
    T.WantRigid t x -> badRigidHint x t

    T.HaveExtFlex x -> badFlex Have x BF_Record
    T.WantExtFlex x -> badFlex Want x BF_Record

    T.HaveExtRigid x   -> badRigid x "a more specific record type"
    T.WantExtRigid x   -> badRigid x "a more specific record type"
    T.BothExtRigid x y -> badRigidRigid x y

    T.RecordMismatch actual expected ->
      let
        green = D.green . D.fromName
        dullyellow = D.dullyellow . D.fromName
      in
      case (actual, expected) of
        ([], [f]) ->
          Hint1 $
            D.toFancyHint ["Looks","like","the",green f,"field","is","missing."]

        ([], _:_:_) | length expected < 4 ->
          Hint1 $
            D.toFancyHint $
              ["Looks","like","fields"] ++ D.commaSep "and" green expected ++ ["are","missing."]

        ([x], [y]) ->
          Hint1 $
            D.toFancyHint $
              ["Seems","like","a","record","field","typo.","Maybe"
              ,dullyellow x,"should","be",green y <> "?"
              ]

        ([f], []) ->
          Hint1 $ D.toFancyHint ["Looks","like","the",dullyellow f,"field","is","unexpected."]

        (_:_:_, []) | length actual < 4 ->
          Hint1 $
            D.toFancyHint $
              ["Looks","like","fields"] ++ D.commaSep "and" dullyellow expected ++ ["are","unexpected."]

        (_, _) ->
            case Suggest.best N.toChars actual expected of
              Nothing ->
                Hint0

              Just (before, after) ->
                Hint1 $
                  D.toFancyHint $
                    ["It","could","be","some","typos","(e.g.","maybe",dullyellow before
                    ,"should","be",green after <> "?),","but","it","seems","like","the"
                    ,"mismatch","is","more","complicated","than","that."
                    ]



-- BAD FLEX HINT


badFlexHint :: Direction -> T.Var -> T.Type -> Hint
badFlexHint d x tipe =
  case tipe of
    T.Lambda _ _ _    -> badFlex d x BF_Function
    T.Infinite        -> Hint0
    T.Error           -> Hint0
    T.Flex  y         -> badFlexFlex x y
    T.Rigid y         -> badRigidFlex y x
    T.Type  h n _ _   -> badFlex d x (BF_Type h n)
    T.Alias h n _ _ _ -> badFlex d x (BF_Alias h n)
    T.Unit            -> badFlex d x BF_Other
    T.Pair   _ _      -> badFlex d x BF_Other
    T.Triple _ _ _    -> badFlex d x BF_Other
    T.Empty           -> badFlex d x BF_Record
    T.Record _        -> badFlex d x BF_Record
    T.ExtRec _ _      -> badFlex d x BF_Record


data BadFlex
  = BF_Function
  | BF_Record
  | BF_Type  ModuleName.Canonical T.Name
  | BF_Alias ModuleName.Canonical T.Name
  | BF_Other


data Direction = Have | Want


badFlex :: Direction -> T.Var -> BadFlex -> Hint
badFlex direction (T.Var _ con) tipe =
  case con of
    T.Any ->
      Hint0

    T.Comparable ->
      let
        compareCertainThings =
          " compare Int, Float, Char, String, lists of comparable values,\
          \ and tuples of comparable values."
      in
      case tipe of
        BF_Function ->
          Hint1 $
            D.toSimpleHint $
              "It is not possible to compare functions, but it may be possible to represent the\
              \ essential details of a function as data (data that is comparable) and then use\
              \ that data as an argument to a separate function later on. That said, I can only"
              ++ compareCertainThings

        BF_Other ->
          Hint1 $
            D.toSimpleHint $
              "I only know how to" ++ compareCertainThings

        BF_Type _ name ->
          HintN
            (
              D.toSimpleHint $
                "I do not know how to compare `" ++ T.nameToChars name ++ "` values. I can only"
                ++ compareCertainThings
            )
            [ D.reflowLink
                "Check out" "comparing-custom-types" "for ideas on how to proceed."
            ]

        BF_Alias _ name ->
          Hint1 $
            D.toSimpleHint $
              "The `" ++ T.nameToChars name ++ "` type alias probably contains values\
              \ that are not comparable. I can only" ++ compareCertainThings

        BF_Record ->
          Hint1 $
            D.toSimpleHint $
              "I do not compare records to avoid confusion about which fields get compared\
              \ first. Since fields do not need to be in any particular order for two records to\
              \ have the same type, the fields would need to be compared in alphabetical order.\
              \ That is already likely to be a “silent error” for a lot of people, but even\
              \ worse, ”alphabetical order” is not the same across all languages! So to make\
              \ sure these ordering details are simple and explicit in all languages, I can only"
              ++ compareCertainThings

    T.Appendable ->
      Hint1 $
        D.toSimpleHint "I only know how to append strings and lists."

    T.CompAppend ->
      Hint1 $
        D.toSimpleHint "Only strings and lists are both comparable and appendable."

    T.Number ->
      case tipe of
        BF_Type h n | T.isString h n ->
          case direction of
            Have -> Hint1 $ D.toFancyHint ["Try","using",D.green "String.fromInt","to","convert","it","to","a","string?"]
            Want -> Hint1 $ D.toFancyHint ["Try","using",D.green "String.toInt","to","convert","it","to","an","integer?"]

        _ ->
          Hint1 $ D.toFancyHint ["Only",D.green "Int","and",D.green "Float","values","work","as","numbers."]


badFlexFlex :: T.Var -> T.Var -> Hint
badFlexFlex (T.Var _ con1) (T.Var _ con2) =
  case (,) <$> likeThis con1 <*> likeThis con2 of
    Nothing ->
      Hint0

    Just (likeThis1, likeThis2) ->
      Hint1 $
        D.toSimpleHint $
          "There are no values in Acadia that are both "
          ++ likeThis1 ++ " and " ++ likeThis2 ++ "."
  where
    likeThis con =
      case con of
        T.Any        -> Nothing
        T.Comparable -> Just "comparable"
        T.Appendable -> Just "appendable"
        T.CompAppend -> Nothing
        T.Number     -> Just "a number"



-- BAD RIGID HINT


badRigidHint :: T.Var -> T.Type -> Hint
badRigidHint x tipe =
  case tipe of
    T.Lambda _ _ _    -> badRigid x "a function"
    T.Infinite        -> Hint0
    T.Error           -> Hint0
    T.Flex  y         -> badRigidFlex  x y
    T.Rigid y         -> badRigidRigid x y
    T.Type  _ n _ _   -> badRigid x $ "a `" ++ T.nameToChars n ++ "` value"
    T.Alias _ n _ _ _ -> badRigid x $ "a `" ++ T.nameToChars n ++ "` value"
    T.Unit            -> badRigid x "a unit value"
    T.Pair   _ _      -> badRigid x "a pair"
    T.Triple _ _ _    -> badRigid x "a triple"
    T.Empty           -> badRigid x "an empty record"
    T.Record _        -> badRigid x "a record"
    T.ExtRec _ _      -> badRigid x "a record"


badRigid :: T.Var -> String -> Hint
badRigid var@(T.Var _ con) aThing =
  HintN
    (
      D.toSimpleHint $
        "Your type annotation uses type variable `" ++ T.varToChars var ++
        "` which means " ++ anyTypeOfThing ++ " can flow through, but your code\
        \ is saying it specifically wants " ++ aThing ++ ". Maybe change your type\
        \ annotation to be more specific? Maybe change the code to be more general?"
    )
    [ D.reflowLink "Read" "type-annotations" "for more advice!"
    ]
  where
    anyTypeOfThing =
      case con of
        T.Any        -> "any value (Int, Float, String, Bool, List, etc.)"
        T.Comparable -> "any comparable value (Int, Float, Char, String, etc.)"
        T.Appendable -> "any String or List"
        T.CompAppend -> "any comparable String or List"
        T.Number     -> "both Int and Float"


badRigidFlex :: T.Var -> T.Var -> Hint
badRigidFlex x (T.Var _ con) =
  case con of
    T.Any        -> Hint0
    T.Comparable -> badRigid x "a `comparable` value"
    T.Appendable -> badRigid x "an `appendable` value"
    T.CompAppend -> badRigid x "a `compappend` value"
    T.Number     -> badRigid x "a `number` value"


badRigidRigid :: T.Var -> T.Var -> Hint
badRigidRigid x y =
  HintN
    (
      D.toSimpleHint $
        "Your type annotation uses `" ++ T.varToChars x ++ "` and `" ++ T.varToChars y ++
        "` as separate type variables. Your code seems to be saying they are the\
        \ same though. Maybe they should be the same in your type annotation?\
        \ Maybe your code uses them in a weird way?"
    )
    [ D.reflowLink "Read" "type-annotations" "for more advice!"
    ]



--------------------------------------------------------------------------------
-- TO EXPR REPORT --------------------------------------------------------------
--------------------------------------------------------------------------------


toExprReport :: Code.Source -> T.Localizer -> A.Region -> Category -> T.Type -> Expected T.Type -> Report.Report
toExprReport source localizer exprRegion category tipe expected =
  case expected of
    NoExpectation expectedType ->
      Report.Report "TYPE MISMATCH" exprRegion [] $
        Code.toSnippet source exprRegion Nothing
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
            TypedBody             -> "body of the `" <> N.toChars name <> "` definition:"

        itIs =
          case subContext of
            TypedIfBranch index   -> "The " <> D.ordinal index <> " branch is"
            TypedCaseBranch index -> "The " <> D.ordinal index <> " branch is"
            TypedBody             -> "The body is"
      in
      Report.Report "TYPE MISMATCH" exprRegion [] $
        Code.toSnippet source exprRegion Nothing $
          ( D.reflow ("Something is off with the " <> thing)
          , typeComparison localizer tipe expectedType
              (addCategory itIs category)
              ("But the type annotation on `" <> N.toChars name <> "` says it should be:")
              []
          )

    FromContext region context expectedType ->
      let
        mismatch (maybeHighlight, problem, thisIs, insteadOf, furtherDetails) =
          Report.Report "TYPE MISMATCH" exprRegion [] $
            Code.toSnippet source region maybeHighlight
              ( D.reflow problem
              , typeComparison localizer tipe expectedType (addCategory thisIs category) insteadOf furtherDetails
              )

        badType (maybeHighlight, problem, thisIs, furtherDetails) =
          Report.Report "TYPE MISMATCH" exprRegion [] $
            Code.toSnippet source region maybeHighlight
              ( D.reflow problem
              , loneType localizer tipe expectedType (D.reflow (addCategory thisIs category)) furtherDetails
              )

        custom maybeHighlight docPair =
          Report.Report "TYPE MISMATCH" exprRegion [] $
            Code.toSnippet source region maybeHighlight docPair
      in
      case context of
        ListEntry index ->
          let ith = D.ordinal index in
          mismatch
          ( Just exprRegion
          , "The " <> ith <> " element of this list does not match all the previous elements:"
          , "The " <> ith <> " element is"
          , "But all the previous elements in the list are:"
          , [ D.link "Hint"
                "Everything in a list must be the same type of value. This way, we never\
                \ run into unexpected values partway through a List.map, List.foldl, etc. Read"
                "custom-types"
                "to learn how to “mix” types."
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
                "custom-types"
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
                "custom-types"
                "to learn how to “mix” types."
            ]
          )

        CallArity maybeFuncName numGivenArgs ->
          Report.Report "TOO MANY ARGS" exprRegion [] $
          Code.toSnippet source region (Just exprRegion) $
          case countArgs tipe of
            0 ->
              let
                thisValue =
                  case maybeFuncName of
                    NoName     -> "This value"
                    FuncName n -> "The `" <> N.toChars n <> "` value"
                    CtorName n -> "The `" <> N.toChars n <> "` value"
                    RcrdName n -> "The `" <> N.toChars n <> "` value"
                    OpName op  -> "The (" <> Op.toChars op <> ") operator"
              in
              ( D.reflow $ thisValue <> " is not a function, but it was given " <> D.args numGivenArgs <> "."
              , D.reflow $ "Are there any missing commas? Or missing parentheses?"
              )

            num ->
              let
                thisFunction =
                  case maybeFuncName of
                    NoName     -> "This function"
                    FuncName n -> "The `" <> N.toChars n <> "` function"
                    CtorName n -> "The `" <> N.toChars n <> "` constructor"
                    RcrdName n -> "The `" <> N.toChars n <> "` constructor"
                    OpName op  -> "The (" <> Op.toChars op <> ") operator"
              in
              ( D.reflow $ thisFunction <> " expects " <> D.args num <> ", but it got " <> show numGivenArgs <> " instead."
              , D.reflow $ "Are there any missing commas? Or missing parentheses?"
              )

        CallArg maybeFuncName index ->
          let
            ith = D.ordinal index

            thisFunction =
              case maybeFuncName of
                NoName     -> "this function"
                FuncName n -> "`" <> N.toChars n <> "`"
                CtorName n -> "`" <> N.toChars n <> "`"
                RcrdName n -> "`" <> N.toChars n <> "`"
                OpName op  -> "(" <> Op.toChars op <> ")"
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
          let
            badRecord fields ext =
              custom (Just fieldRegion)
                ( D.reflow $
                    "This "
                    <> maybe "" (\n -> "`" <> N.toChars n <> "`") maybeName
                    <> " record does not have a `" <> N.toChars field <> "` field:"
                ,
                  case Suggest.sort (N.toChars field) (N.toChars . fst) fields of
                    [] ->
                      D.reflow "In fact, it is a record with NO fields!"

                    f:fs ->
                      D.stack
                        [ D.reflow $
                            "This is usually a typo. Here are the "
                            <> maybe "" (\n -> "`" <> N.toChars n <> "`") maybeName
                            <> " fields that are most similar:"
                        , toNearbyRecord localizer f fs ext
                        , D.fillSep
                            ["So","maybe",D.dullyellow (D.fromName field)
                            ,"should","be",D.green (D.fromName (fst f)) <> "?"
                            ]
                        ]
                )
          in
          case T.iteratedDealias tipe of
            T.Empty       -> badRecord [] Nothing
            T.Record fs   -> badRecord (NEM.toList (F.toNEM fs)) Nothing
            T.ExtRec fs e -> badRecord (NEM.toList (F.toNEM fs)) (Just e)
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
          let
            badRecord actualFields ext =
              case Map.lookupMin (Map.difference (NEM.toDict expectedFields) actualFields) of
                Nothing ->
                  mismatch
                  ( Nothing
                  , "Something is off with this record update:"
                  , "The `" <> N.toChars record <> "` record is"
                  , "But this update needs it to be compatable with:"
                  , [ D.reflow
                        "Do you mind creating an <http://sscce.org/> that produces this error message and\
                        \ sharing it at <https://github.com/elm/error-message-catalog/issues> so we\
                        \ can try to give better advice here?"
                    ]
                  )

                Just (field, Can.Update_ fieldRegion _) ->
                  let
                    rStr = "`" <> N.toChars record <> "`"
                    fStr = "`" <> N.toChars field <> "`"
                  in
                  custom (Just fieldRegion)
                    ( D.reflow $
                        "The " <> rStr <> " record does not have a " <> fStr <> " field:"
                    , case Suggest.sort (N.toChars field) (N.toChars . fst) (Map.toList actualFields) of
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
          in
          case T.iteratedDealias tipe of
            T.Empty       -> badRecord Map.empty Nothing
            T.Record fs   -> badRecord (NEM.toDict (F.toNEM fs)) Nothing
            T.ExtRec fs e -> badRecord (NEM.toDict (F.toNEM fs)) (Just e)
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
          , "I cannot update the `" <> N.toChars field <> "` field like this:"
          , "You are trying to update `" <> N.toChars field <> "` to be"
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


toNearbyRecord :: T.Localizer -> (N.Name, T.Type) -> [(N.Name, T.Type)] -> Maybe T.Ext -> D.Doc
toNearbyRecord localizer f fs ext =
  D.indent 4 $
    if length fs <= 3
    then T.vrecord (map fieldToDocs (f:fs)) (fmap extToDoc ext)
    else T.vrecordSnippet (fieldToDocs f) (map fieldToDocs (take 3 fs))
  where
    fieldToDocs (name, tipe) =
      ( D.fromName name
      , T.errToDoc localizer T.None tipe
      )

    extToDoc e =
      case e of
        T.ExtFlex  x -> D.fromVar x
        T.ExtRigid x -> D.fromVar x



-- OP LEFT


opLeftToDocs :: T.Localizer -> Category -> Op.Name -> T.Type -> T.Type -> (D.Doc, D.Doc)
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
          "The left argument of (" <> Op.toChars op <> ") is causing problems:"
      , typeComparison localizer tipe expected
          (addCategory "The left argument is" category)
          ("But (" <> Op.toChars op <> ") needs the left argument to be:")
          []
      )



-- OP RIGHT


data RightDocs
  = EmphBoth (D.Doc, D.Doc)
  | EmphRight (D.Doc, D.Doc)


opRightToDocs :: T.Localizer -> Category -> Op.Name -> T.Type -> T.Type -> RightDocs
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
            "But (<|) is piping it to a function that expects:"
            []
        )

    "|>" ->
      case (tipe, expected) of
        (T.Lambda expectedArgType _ _, T.Lambda argType _ _) ->
          EmphRight
            ( D.reflow $ "This function cannot handle the argument sent through the (|>) pipe:"
            , typeComparison localizer argType expectedArgType
                "The argument is:"
                "But (|>) is piping it to a function that expects:"
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


badOpRightFallback :: T.Localizer -> Category -> Op.Name -> T.Type -> T.Type -> RightDocs
badOpRightFallback localizer category op tipe expected =
  EmphRight
    ( D.reflow $
        "The right argument of (" <> Op.toChars op <> ") is causing problems."
    , typeComparison localizer tipe expected
        (addCategory "The right argument is" category)
        ("But (" <> Op.toChars op <> ") needs the right argument to be:")
        [ D.toSimpleHint $
            "With operators like (" ++ Op.toChars op ++ ") I always check the left\
            \ side first. If it seems fine, I assume it is correct and check the right\
            \ side. So the problem may be in how the left and right arguments interact!"
        ]
    )


isInt    :: T.Type -> Bool; isInt    t = case t of { T.Type h n _ []  -> T.isInt    h n ; _ -> False }
isFloat  :: T.Type -> Bool; isFloat  t = case t of { T.Type h n _ []  -> T.isFloat  h n ; _ -> False }
isString :: T.Type -> Bool; isString t = case t of { T.Type h n _ []  -> T.isString h n ; _ -> False }
isList   :: T.Type -> Bool; isList   t = case t of { T.Type h n _ [_] -> T.isList   h n ; _ -> False }



-- BAD CONS


badConsRight :: T.Localizer -> Category -> T.Type -> T.Type -> RightDocs
badConsRight localizer category tipe expected =
  case tipe of
    T.Type h1 n1 _ [actualElement] | T.isList h1 n1 ->
      case expected of
        T.Type h2 n2 _ [expectedElement] | T.isList h2 n2 ->
          EmphBoth
            ( D.reflow "I am having trouble with this (::) operator:"
            , typeComparison localizer expectedElement actualElement
                "The left side of (::) is:"
                "But you are trying to put that into a list filled with:"
                ( case expectedElement of
                    T.Type h n _ [_] | T.isList h n ->
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
    T.Type h n _ _
      | T.isInt    h n -> ANumber "Int" "String.fromInt"
      | T.isFloat  h n -> ANumber "Float" "String.fromFloat"
      | T.isString h n -> AString
      | T.isList   h n -> AList

    T.Flex (T.Var _ T.Number) -> ANumber "number" "String.fromInt"

    _ -> AOther


badAppendLeft :: T.Localizer -> Category -> T.Type -> T.Type -> (D.Doc, D.Doc)
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


badAppendRight :: T.Localizer -> Category -> T.Type -> T.Type -> RightDocs
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


badCast :: Op.Name -> ThisThenThat -> RightDocs
badCast op thisThenThat =
  EmphBoth
    ( D.reflow $
        "I need both sides of (" <> Op.toChars op <> ") to be the exact same type. Both Int or both Float."
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


badListAdd :: T.Localizer -> Category -> String -> T.Type -> T.Type -> (D.Doc, D.Doc)
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


badListMul :: T.Localizer -> Category -> String -> T.Type -> T.Type -> (D.Doc, D.Doc)
badListMul localizer category direction tipe expected =
  badMath localizer category "Multiplication" direction "*" tipe expected
    [
      D.toFancyHint
        [ "Maybe", "you", "want"
        , D.green "List.repeat"
        , "to", "build","a","list","of","repeated","values?"
        ]
    ]


badMath :: T.Localizer -> Category -> String -> String -> String -> T.Type -> T.Type -> [D.Doc] -> (D.Doc, D.Doc)
badMath localizer category operation direction op tipe expected otherHints =
  (
    D.reflow $
      operation ++ " does not work with this value:"
  ,
    loneType localizer tipe expected
      (D.reflow (addCategory ("The " <> direction <> " side of (" <> op <> ") is") category))
      ( [ D.fillSep
            ["But","(" <> D.fromChars op <> ")","only","works","with"
            ,D.dullyellow "Int","and",D.dullyellow "Float","values."
            ]
        ]
        ++ otherHints
      )
  )


badFDiv :: T.Localizer -> D.Doc -> T.Type -> T.Type -> (D.Doc, D.Doc)
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


badIDiv :: T.Localizer -> D.Doc -> T.Type -> T.Type -> (D.Doc, D.Doc)
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


badBool :: T.Localizer -> D.Doc -> D.Doc -> T.Type -> T.Type -> (D.Doc, D.Doc)
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


badCompLeft :: T.Localizer -> Category -> String -> String -> T.Type -> T.Type -> (D.Doc, D.Doc)
badCompLeft localizer category op direction tipe expected =
  (
    D.reflow $
      "I cannot do a comparison with this value:"
  ,
    loneType localizer tipe expected
      (D.reflow (addCategory ("The " <> direction <> " side of (" <> op <> ") is") category))
      [ D.fillSep
          ["But","(" <> D.fromChars op <> ")","only","works","on"
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


badCompRight :: T.Localizer -> String -> T.Type -> T.Type -> RightDocs
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


badEquality :: T.Localizer -> String -> T.Type -> T.Type -> RightDocs
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


toInfiniteReport :: Code.Source -> T.Localizer -> A.Region -> N.Name -> T.Type -> Report.Report
toInfiniteReport source localizer region name overallType =
  Report.Report "INFINITE TYPE" region [] $
    Code.toSnippet source region Nothing
      (
        D.reflow $
          "I am inferring a weird self-referential type for " <> N.toChars name <> ":"
      ,
        D.stack
          [ D.reflow $
              "Here is my best effort at writing down the type. You will see ∞ for\
              \ parts of the type that repeat something already printed out infinitely."
          , D.indent 4 (D.dullyellow (T.errToDoc localizer T.None overallType))
          , D.reflowLink
              "Staring at this type is usually not so helpful, so I recommend reading the hints at"
              "infinite-type"
              "to get unstuck!"
          ]
      )
