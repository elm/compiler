{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Reporting.Error.Type
  ( Error(..)
  , Mismatch(..)
  , Hint(..)
  , Reason(..)
  , SpecificThing(..)
  , PatternContext(..)
  , PatternCategory(..)
  , toReport
  , flipReason
  )
  where

import Control.Arrow (second)
import qualified Data.Map as Map
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Monoid ((<>))

import qualified AST.Module.Name as ModuleName
import qualified AST.Type as Type
import qualified AST.Variable as Var
import qualified Elm.Name as N
import qualified Reporting.Region as R
import qualified Reporting.Render.Type as RenderType
import qualified Reporting.Report as Report
import qualified Reporting.Helpers as Help
import Reporting.Helpers
  ( Doc, capitalize, dullyellow, functionName, i2t
  , indent, ordinalize, reflowParagraph, stack, text
  )



-- ERRORS


data Error
  = Mismatch Mismatch
  | BadMain Type.Canonical
  | BadFlags Type.Canonical (Maybe Text)
  | InfiniteType (Either Hint Text) Type.Canonical


data Mismatch =
  MismatchInfo
    { _hint :: Hint
    , _leftType :: Type.Canonical
    , _rightType :: Type.Canonical
    , _decoders :: Bool
    , _reason :: Maybe Reason
    }


data Reason
  = BadFields [(Text, Maybe Reason)]
  | MessyFields [Text] [Text] [Text]
  | IntFloat
  | MissingArgs Int
  | RigidClash Text Text
  | NotPartOfSuper Type.Super
  | RigidVarTooGeneric Text SpecificThing
  | RigidSuperTooGeneric Type.Super Text SpecificThing


data SpecificThing
  = SpecificSuper Type.Super
  | SpecificType N.Name
  | SpecificFunction
  | SpecificRecord
  | SpecificUnit
  | SpecificTuple


data Hint
  = CaseBranch Int R.Region
  | Case
  | IfCondition
  | IfBranches
  | MultiIfBranch Int R.Region
  | If
  | List
  | ListElement Int R.Region
  | BinopLeft ModuleName.Canonical N.Name R.Region
  | BinopRight ModuleName.Canonical N.Name R.Region
  | Binop ModuleName.Canonical N.Name
  | Function (Maybe Var.Canonical)
  | UnexpectedArg (Maybe Var.Canonical) Int Int R.Region
  | FunctionArity (Maybe Var.Canonical) Int Int R.Region
  | ReturnType Text Int Int R.Region
  | Instance Text
  | Literal Text
  | Pattern PatternContext PatternCategory
  | PatternCons
  | Shader
  | Lambda
  | Accessor Text
  | Access (Maybe Text) Text
  | Record
  | Unit
  | Tuple
  -- effect manager problems
  | Manager Text
  | State Text
  | SelfMsg


data PatternContext
  = PatternArg N.Name Int
  | PatternTail
  | PatternList Int
  | PatternUnknown


data PatternCategory
  = PRecord
  | PUnit
  | PTuple
  | PList
  | PCtor N.Name



-- TO REPORT


toReport :: RenderType.Localizer -> Error -> Report.Report
toReport localizer err =
  case err of
    Mismatch info ->
        mismatchToReport localizer info

    InfiniteType context overallType ->
        infiniteTypeToReport localizer context overallType

    BadMain tipe ->
        Report.report
          "BAD MAIN TYPE"
          Nothing
          "The `main` value has an unsupported type."
          ( stack
              [ reflowParagraph $
                "I need Html, Svg, or a Program so I have something to render on\
                \ screen, but you gave me:"
              , indent 4 (RenderType.toDoc localizer tipe)
              ]
          )

    BadFlags tipe maybeMessage ->
      let
        context =
          maybe "" (" the following " <> ) maybeMessage
      in
        Report.report
          "BAD FLAGS"
          Nothing
          ("Your `main` is demanding an unsupported type as a flag."
          )
          ( stack
              [ text ("The specific unsupported type is" <> context <> ":")
              , indent 4 (RenderType.toDoc localizer tipe)
              , text "The types of values that can flow through in and out of Elm include:"
              , indent 4 $ reflowParagraph $
                  "Ints, Floats, Bools, Strings, Maybes, Lists, Arrays,\
                  \ Tuples, Json.Values, and concrete records."
              ]
          )



-- TYPE MISMATCHES


mismatchToReport :: RenderType.Localizer -> Mismatch -> Report.Report
mismatchToReport localizer (MismatchInfo hint leftType rightType decoders maybeReason) =
  let
    report =
      Report.report "TYPE MISMATCH"

    typicalHints =
      maybe id (:) (reasonToDoc <$> maybeReason) $
        if decoders then [ jsonDecoderHint ] else []

    cmpHint leftWords rightWords extraHints =
      comparisonHint localizer leftType rightType leftWords rightWords
        (typicalHints <> map Help.toSimpleHint extraHints)
  in
  case hint of
    CaseBranch branchNumber region ->
        report
          (Just region)
          ( "The " <> ordinalPair branchNumber
            <> " branches of this `case` produce different types of values."
          )
          ( cmpHint
              ("The " <> ordinalize (branchNumber - 1) <> " branch has this type:")
              ("But the " <> ordinalize branchNumber <> " is:")
              [ "All branches in a `case` must have the same type. So no matter\
                \ which one we take, we always get back the same type of value."
              ]
          )

    Case ->
        report
          Nothing
          ( "All the branches of this case-expression are consistent, but the overall\n"
            <> "type does not match how it is used elsewhere."
          )
          ( cmpHint
              "The `case` evaluates to something of type:"
              "Which is fine, but the surrounding context wants it to be:"
              []
          )

    IfCondition ->
        report
          Nothing
          "This condition does not evaluate to a boolean value, True or False."
          ( cmpHint
              "You have given me a condition with this type:"
              "But I need it to be:"
              [ "Elm does not have \"truthiness\" such that ints and strings and lists\
                \ are automatically converted to booleans. Do that conversion explicitly."
              ]
          )

    IfBranches ->
        report
          Nothing
          "The branches of this `if` produce different types of values."
          ( cmpHint
              "The `then` branch has type:"
              "But the `else` branch is:"
              [ "These need to match so that no matter which branch we take, we\
                \ always get back the same type of value."
              ]
          )

    MultiIfBranch branchNumber region ->
        report
          (Just region)
          ( "The " <> ordinalPair branchNumber
            <> " branches of this `if` produce different types of values."
          )
          ( cmpHint
              ("The " <> ordinalize (branchNumber - 1) <> " branch has this type:")
              ("But the "<> ordinalize branchNumber <> " is:")
              [ "All the branches of an `if` need to match so that no matter which\
                \ one we take, we get back the same type of value overall."
              ]
          )

    If ->
        report
          Nothing
          "All the branches of this `if` are consistent, but the overall\
          \ type does not match how it is used elsewhere."
          ( cmpHint
              "The `if` evaluates to something of type:"
              "Which is fine, but the surrounding context wants it to be:"
              []
          )

    ListElement elementNumber region ->
        report
          (Just region)
          ("The " <> ordinalPair elementNumber <> " entries in this list are different types of values.")
          ( cmpHint
              ("The " <> ordinalize (elementNumber - 1) <> " entry has this type:")
              ("But the "<> ordinalize elementNumber <> " is:")
              [ "Every entry in a list needs to be the same type of value.\
                \ This way you never run into unexpected values partway through.\
                \ To mix different types in a single list, create a \"union type\" as\
                \ described in: <http://guide.elm-lang.org/types/union_types.html>"
              ]
          )

    List ->
        report
          Nothing
          ( "All the elements in this list are the same type, but the overall\
            \ type does not match how it is used elsewhere."
          )
          ( cmpHint
              "The list has type:"
              "Which is fine, but the surrounding context wants it to be:"
              []
          )

    BinopLeft home op region ->
        report
          (Just region)
          ("The left argument of (" <> op <> ") is causing a type mismatch.")
          ( cmpHint
              ("(" <> op <> ") is expecting the left argument to be a:")
              "But the left argument is:"
              (binopHint home op leftType rightType)
          )

    BinopRight home op region ->
        report
          (Just region)
          ("The right side of (" <> op <> ") is causing a type mismatch.")
          ( cmpHint
              ("(" <> op <> ") is expecting the right side to be a:")
              "But the right side is:"
              ( binopHint home op leftType rightType
                ++
                [ "With operators like (" <> op <> ") I always check the left\
                  \ side first. If it seems fine, I assume it is correct and check the right\
                  \ side. So the problem may be in how the left and right arguments interact."
                ]
              )
          )

    Binop _ op ->
        report
          Nothing
          ( "The two arguments to (" <> op <>
            ") are fine, but the overall type of this expression\
            \ does not match how it is used elsewhere."
          )
          ( cmpHint
              "The result of this binary operation is:"
              "Which is fine, but the surrounding context wants it to be:"
              []
          )

    Function maybeName ->
        report
          Nothing
          ( "The return type of " <> maybeFuncName maybeName <> " is being used in unexpected ways."
          )
          ( cmpHint
              "The function results in this type of value:"
              "Which is fine, but the surrounding context wants it to be:"
              []
          )

    UnexpectedArg maybeName 1 1 region ->
        report
          (Just region)
          ("The argument to " <> maybeFuncName maybeName <> " is causing a mismatch.")
          ( cmpHint
              (capitalize (maybeFuncName maybeName) <> " is expecting the argument to be:")
              "But it is:"
              []
          )

    UnexpectedArg maybeName index _totalArgs region ->
        report
          (Just region)
          ( "The " <> ordinalize index <> " argument to " <> maybeFuncName maybeName
            <> " is causing a mismatch."
          )
          ( cmpHint
              ( capitalize (maybeFuncName maybeName) <> " is expecting the "
                <> ordinalize index <> " argument to be:"
              )
              "But it is:"
              ( if index == 1 then
                  []
                else
                  [ "I always figure out the type of arguments from left to right. If an argument\
                    \ is acceptable when I check it, I assume it is \"correct\" in subsequent checks.\
                    \ So the problem may actually be in how previous arguments interact with the "
                    <> ordinalize index <> "."
                  ]
              )
          )

    FunctionArity maybeName 0 actual region ->
        let
          args =
            if actual == 1 then "an argument" else i2t actual <> " arguments"

          preHint =
            case maybeName of
              Nothing ->
                  "You are giving " <> args <> " to something that is not a function!"

              Just name ->
                  varName name <> " is not a function, but you are giving it " <> args <> "!"
        in
          report
            (Just region)
            preHint
            (text "Maybe you forgot some parentheses? Or a comma?")

    FunctionArity maybeName expected actual region ->
      report
          (Just region)
          ( capitalize (maybeFuncName maybeName) <> " is expecting "
            <> Help.args expected <> ", but was given " <> i2t actual <> "."
          )
          (text "Maybe you forgot some parentheses? Or a comma?")

    ReturnType name typeArity argArity region ->
      if typeArity == 0 || argArity == 0 then
        report
          (Just region)
          ("The definition of " <> functionName name <> " does not match its type annotation.")
          ( cmpHint
              ( "The type annotation for " <> functionName name <> " says it is a:"
              )
              "But the definition (shown above) is a:"
              (arityHint typeArity argArity)
          )

      else
        report
          (Just region)
          ("The definition of " <> functionName name <> " does not match its type annotation.")
          ( cmpHint
              ( "The type annotation for " <> functionName name <> " says it always returns:"
              )
              "But the returned value (shown above) is a:"
              (arityHint typeArity argArity)
          )

    Instance var ->
      let
        name =
          functionName var
      in
        report
          Nothing
          (name <> " is being used in an unexpected way.")
          ( cmpHint
              ("Based on its definition, " <> name <> " has this type:")
              "But you are trying to use it as:"
              []
          )

    Literal name ->
        report
          Nothing
          ( "This " <> name <> " value is being used as if it is some other type of value."
          )
          ( cmpHint
              ("The " <> name <> " definitely has this type:")
              ("But it is being used as:")
              []
          )

    Pattern context category ->
        let (overview, lineOne, lineTwo) = toPatternMessage context category in
        report Nothing overview (cmpHint lineOne lineTwo [])

    PatternCons ->
        report Nothing "This list pattern is mixing types:" $
          cmpHint
            "The head of the list matches this type of value:"
            "But the tail of the list matches:"
            []

    Shader ->
        report
          Nothing
          "There is some problem with this GLSL shader."
          ( cmpHint
              "The shader block has this type:"
              "Which is fine, but the surrounding context wants it to be:"
              []
          )

    Lambda ->
        report
          Nothing
          "This function is being used in an unexpected way."
          ( cmpHint
              "The function has type:"
              "But you are trying to use it as:"
              []
          )

    Accessor field ->
      report Nothing
        ("The ." <> field <> " accessor is being used in an unexpected way.")
        ( cmpHint
            "It has type:"
            "But you are trying to use it as:"
            []
        )

    Access (Just body) field ->
      let
        header = "`" <> body <> "` does not have a field named `" <> field <> "`."
      in
        report Nothing header $ stack $
          [ reflowParagraph $ "The type of `" <> body <> "` is:"
          , indent 4 $ dullyellow $ RenderType.toDoc localizer leftType
          , reflowParagraph $ "Which does not contain a field named `" <> field <> "`."
          ]
          ++ typicalHints

    Access Nothing field ->
      let
        header = "Cannot access a field named `" <> field <> "`."
      in
        report Nothing header $ stack $
          [ reflowParagraph $ "You are trying to get `" <> field <> "` from a value with this type:"
          , indent 4 $ dullyellow $ RenderType.toDoc localizer leftType
          , reflowParagraph $ "It is not in there!"
          ]
          ++ typicalHints

    Record ->
        report
          Nothing
          "This record is being used in an unexpected way."
          ( cmpHint
              "The record has type:"
              "But you are trying to use it as:"
              []
          )

    Unit ->
        report
          Nothing
          "This “unit” value is being used in an unexpected way."
          ( cmpHint
              "It has type:"
              "But you are trying to use it as:"
              []
          )

    Tuple ->
        report
          Nothing
          "This tuple is being used in an unexpected way."
          ( cmpHint
              "The tuple has type:"
              "But you are trying to use it as:"
              []
          )

    Manager name ->
        report
          Nothing
          ("The `" <> name <> "` in your effect manager has a weird type.")
          ( cmpHint
              ("Your `" <> name <> "` function has this type:")
              "But it needs to have a type like this:"
              [ "You can read more about setting up effect managers properly here:\
                \ <http://guide.elm-lang.org/effect_managers/>"
              ]
          )

    State name ->
        report
          Nothing
          ( "Your effect manager creates a certain type of state with `init`, but your `"
            <> name <> "` function expects a different kind of state."
          )
          ( cmpHint
              "The state created by `init` has this type:"
              ("But `" <> name <> "` expects state of this type:")
              [ "Make the two state types match and you should be all set! More info here:\
                \ <http://guide.elm-lang.org/effect_managers/>"
              ]
          )

    SelfMsg ->
        report
          Nothing
          "Effect managers can send messages to themselves, but `onEffects` and `onSelfMsg` are defined for different types of self messages."
          ( cmpHint
              "The `onEffects` function can send this type of message:"
              "But the `onSelfMsg` function receives this type:"
              [ "Make the two message types match and you should be all set! More info here:\
                \ <http://guide.elm-lang.org/effect_managers/>"
              ]
          )



-- PATTERN REPORT


toPatternMessage :: PatternContext -> PatternCategory -> (Text, Text, Text)
toPatternMessage context category =
  case context of
    PatternArg name index ->
      toPatternHelp category $
        "But as the " <> ordinalize index
        <> " value of a `" <> name <> "` pattern, it must be:"

    PatternTail ->
      toPatternHelp category $
        "But as the pattern after a (::), it must be a list like: "

    PatternList index ->
      ( "The patterns in this list match different types of values:"
      , "The " <> ordinalize index <> " pattern matches type:"
      , "But all the previous patterns in the list match:"
      )

    PatternUnknown ->
      toPatternHelp category $
        "But the values flowing through here need to be:"


toPatternHelp :: PatternCategory -> Text -> (Text, Text, Text)
toPatternHelp category lineTwo =
  case category of
    PRecord ->
      ( "This record pattern will cause problems:"
      , "It matches record values with this type:"
      , lineTwo
      )

    PUnit ->
      ( "This “unit” pattern will cause problems:"
      , "It matches unit values with this type:"
      , lineTwo
      )

    PTuple ->
      ( "This tuple pattern will cause problems:"
      , "It matches tuples with this type:"
      , lineTwo
      )

    PList ->
      ( "This list pattern will cause problems:"
      , "It matches lists with this type:"
      , lineTwo
      )

    PCtor name ->
      ( "This `" <> name <> "` value will cause problems:"
      , "It matches `" <> name <> "` values with this type:"
      , lineTwo
      )



-- COMPARISON HINT


comparisonHint
    :: RenderType.Localizer
    -> Type.Canonical
    -> Type.Canonical
    -> Text
    -> Text
    -> [Doc]
    -> Doc
comparisonHint localizer leftType rightType leftWords rightWords finalHints =
  let
    (leftDoc, rightDoc) =
      RenderType.diffToDocs localizer leftType rightType
  in
    stack $
      [ reflowParagraph leftWords
      , indent 4 leftDoc
      , reflowParagraph rightWords
      , indent 4 rightDoc
      ]
      ++
      finalHints



-- JSON DECODER HINT


jsonDecoderHint :: Doc
jsonDecoderHint =
  Help.toSimpleHint $
    "New to JSON decoders? They are tough at first, so DEFINITELY learn more here: "
    <> Help.hintLink "json-decoders"



-- BINOP HINTS


binopHint :: ModuleName.Canonical -> N.Name -> Type.Canonical -> Type.Canonical -> [Text]
binopHint home op leftType rightType =
  let
    leftString = show (RenderType.toDoc Map.empty leftType)
    rightString = show (RenderType.toDoc Map.empty rightType)
  in
  if home == ModuleName.basics && op == "+" && elem "String" [leftString, rightString] then
      [ "To append strings in Elm, you need to use the (++) operator, not (+).\
        \ <http://package.elm-lang.org/packages/elm-lang/core/latest/Basics#++>"
      ]

  else if home == ModuleName.basics && op == "/" && elem "Int" [leftString, rightString] then
      [ "The (/) operator is specifically for floating point division, and (//) is\
        \ for integer division. You may need to do some conversions between ints and\
        \ floats to get both arguments matching the division operator you want."
      ]

  else
      []



-- ARITY HINTS


arityHint :: Int -> Int -> [Text]
arityHint typeArity argArity =
  if typeArity == argArity then
    []

  else
    [ "The type annotation says there " <> sayArgs typeArity <>
      ", but there " <> sayArgs argArity <>
      " named in the definition. It is best practice for each argument\
      \ in the type to correspond to a named argument in the definition,\
      \ so try that first!"
    ]


sayArgs :: Int -> Text
sayArgs n =
  case n of
    0 ->
      "are NO arguments"

    1 ->
      "is 1 argument"

    _ ->
      "are " <> i2t n <> " arguments"



-- MISMATCH HELPERS


ordinalPair :: Int -> Text
ordinalPair number =
  ordinalize (number - 1) <> " and " <> ordinalize number


varName :: Var.Canonical -> Text
varName (Var.Canonical _ opName) =
  functionName opName


maybeFuncName :: Maybe Var.Canonical -> Text
maybeFuncName maybeVar =
  case maybeVar of
    Nothing ->
      "this function"

    Just var ->
      "function " <> varName var



-- MISMTACH REASONS


flipReason :: Reason -> Reason
flipReason reason =
  case reason of
    BadFields fields ->
        BadFields (map (second (fmap flipReason)) fields)

    MessyFields both left right ->
        MessyFields both right left

    IntFloat ->
        IntFloat

    MissingArgs num ->
        MissingArgs num

    RigidClash a b ->
        RigidClash b a

    NotPartOfSuper super ->
        NotPartOfSuper super

    RigidVarTooGeneric name specific ->
        RigidVarTooGeneric name specific

    RigidSuperTooGeneric super name specific ->
        RigidSuperTooGeneric super name specific


reasonToDoc :: Reason -> Doc
reasonToDoc reason =
  case collectFields reason of
    Nothing ->
      let
        (firstLine, details) =
          reasonToDocHelp reason
      in
      Help.vcat $ Help.toSimpleHint firstLine : map (indent 4) details

    Just (fields, maybeDeepReason) ->
      let
        recordHint elaboration =
          [ "Problem", "in", "the", dullyellow (text (Text.intercalate "." fields)) <> "field." ]
          ++
          map text (Text.words elaboration)
      in
      case reasonToDocHelp <$> maybeDeepReason of
        Nothing ->
          Help.toFancyHint (recordHint badFieldElaboration)

        Just (firstLine, details) ->
          Help.vcat $ Help.toFancyHint (recordHint firstLine) : map (indent 4) details



-- COLLECT FIELDS


collectFields :: Reason -> Maybe ([Text], Maybe Reason)
collectFields reason =
  case reason of
    BadFields [(field, maybeReason)] ->
      Just (collectFieldsHelp [field] maybeReason)

    _ ->
      Nothing


collectFieldsHelp :: [Text] -> Maybe Reason -> ([Text], Maybe Reason)
collectFieldsHelp revFields maybeReason =
  case maybeReason of
    Just (BadFields [(field, deeperReason)]) ->
      collectFieldsHelp (field : revFields) deeperReason

    _ ->
      ( reverse revFields, maybeReason )


reasonToDocHelp :: Reason -> (Text, [Doc])
reasonToDocHelp reason =
  let
    simpleDoc msg =
      ( msg, [] )
  in
  case reason of
    BadFields fields ->
      simpleDoc $
        "I am seeing issues with the "
        <> Help.commaSep (map fst fields) <> " fields. "
        <> badFieldElaboration

    MessyFields both leftOnly rightOnly ->
        messyFieldsHelp both leftOnly rightOnly

    IntFloat ->
        simpleDoc $
          "Elm does not automatically convert between Ints and Floats. Use\
          \ `toFloat` and `round` to do specific conversions.\
          \ <http://package.elm-lang.org/packages/elm-lang/core/latest/Basics#toFloat>"

    MissingArgs num ->
        simpleDoc $
          "It looks like a function needs " <> Help.moreArgs num <> "."

    RigidClash name1 name2 ->
        simpleDoc $
          "Your type annotation uses `" <> name1 <> "` and `" <> name2 <>
          "` as DIFFERENT type variables. By using separate names, you are\
          \ claiming the values can be different, but the code suggests that\
          \ they must be the SAME based on how they are used. Maybe these two\
          \ type variables in your type annotation should just be one? Maybe\
          \ your code uses them in a weird way? More help at: "
          <> Help.hintLink "type-annotations"

    NotPartOfSuper Type.Number ->
        simpleDoc "Only ints and floats are numbers."

    NotPartOfSuper Type.Comparable ->
        simpleDoc "Only ints, floats, chars, strings, lists, and tuples are comparable."

    NotPartOfSuper Type.Appendable ->
        simpleDoc "Only strings and lists are appendable."

    NotPartOfSuper Type.CompAppend ->
        simpleDoc "Only strings and lists are both comparable and appendable."

    RigidVarTooGeneric name specific ->
        simpleDoc $ rigidTooGenericHelp "ANY type of value" name specific

    RigidSuperTooGeneric Type.Number name specific ->
        simpleDoc $ rigidTooGenericHelp "BOTH Ints and Floats" name specific

    RigidSuperTooGeneric Type.Comparable name specific ->
        simpleDoc $ rigidTooGenericHelp "ANY comparable value" name specific

    RigidSuperTooGeneric Type.Appendable name specific ->
        simpleDoc $ rigidTooGenericHelp "BOTH Strings and Lists" name specific

    RigidSuperTooGeneric Type.CompAppend name specific ->
        simpleDoc $ rigidTooGenericHelp "Strings and ANY comparable List" name specific


rigidTooGenericHelp :: Text -> Text -> SpecificThing -> Text
rigidTooGenericHelp manyTypesOfValues name specific =
  "The type variable `" <> name <> "` in your type annotation suggests that"
  <> manyTypesOfValues <> " can flow through, but the code suggests that it must be "
  <> specificThingToText specific <> " based on how it is used. Maybe make your\
  \ type annotation more specific? Maybe the code has a problem? More help at: "
  <> Help.hintLink "type-annotations"


specificThingToText :: SpecificThing -> Text
specificThingToText specific =
  case specific of
    SpecificSuper Type.Number ->
      "a number"

    SpecificSuper Type.Comparable ->
      "comparable"

    SpecificSuper Type.Appendable ->
      "appendable"

    SpecificSuper Type.CompAppend ->
      "comparable AND appendable"

    SpecificType name ->
      if Text.isInfixOf (Text.take 1 name) "AEIOU" then
        "an " <> name

      else
        "a " <> name

    SpecificRecord ->
      "a record"

    SpecificFunction ->
      "a function"

    SpecificUnit ->
      "a unit value"

    SpecificTuple ->
      "a tuple"


badFieldElaboration :: Text
badFieldElaboration =
  "I always figure out field types in alphabetical order. If a field\
  \ seems fine, I assume it is “correct” in subsequent checks.\
  \ So the problem may actually be a weird interaction with previous fields."


messyFieldsHelp :: [Text] -> [Text] -> [Text] -> (Text, [Doc])
messyFieldsHelp both leftOnly rightOnly =
  error "TODO messyFieldsHelp" both leftOnly rightOnly

{-
  case (leftOnly, rightOnly) of
    ([], [missingField]) ->
      oneMissingField both missingField

    ([missingField], []) ->
      oneMissingField both missingField

    ([], missingFields) ->
      manyMissingFields both missingFields

    (missingFields, []) ->
      manyMissingFields both missingFields

    _ ->
      let
        typoPairs =
          case Help.findTypoPairs leftOnly rightOnly of
            [] ->
              Help.findTypoPairs (both ++ leftOnly) (both ++ rightOnly)

            pairs ->
              pairs
      in
        if null typoPairs then
          ( "The record fields do not match up. One has "
            <> Help.commaSep leftOnly <> ". The other has "
            <> Help.commaSep rightOnly <> "."
          , []
          )

        else
          ( "The record fields do not match up. Maybe you made one of these typos?"
          , typoDocs "<->" typoPairs
          )


oneMissingField :: [Text] -> Text -> ( Text, [Doc] )
oneMissingField knownFields missingField =
  case Help.findPotentialTypos knownFields missingField of
    [] ->
      ( "Looks like a record is missing the `" <> missingField <> "` field."
      , []
      )

    [typo] ->
      ( "Looks like a record is missing the `" <> missingField
        <> "` field. Maybe it is a typo?"
      , typoDocs "->" [(missingField, typo)]
      )

    typos ->
      ( "Looks like a record is missing the `" <> missingField
        <> "` field. It is close to names like "
        <> Help.commaSep typos <> " so maybe it is a typo?"
      , []
      )


manyMissingFields :: [Text] -> [Text] -> (Text, [Doc])
manyMissingFields knownFields missingFields =
  case Help.findTypoPairs missingFields knownFields of
    [] ->
      ( "Looks like a record is missing these fields: "
        <> Help.commaSep missingFields
      , []
      )

    typoPairs ->
      ( "Looks like a record is missing these fields: "
        <> Help.commaSep missingFields
        <> ". Potential typos include:"
      , typoDocs "->" typoPairs
      )


typoDocs :: Text -> [(Text, Text)] -> [Doc]
typoDocs arrow typoPairs =
  let
    maxLen =
      maximum (map (Text.length . fst) typoPairs)
  in
    text "" : map (padTypo arrow maxLen) typoPairs


padTypo :: Text -> Int -> (Text, Text) -> Doc
padTypo arrow maxLen (missingField, knownField) =
  text (Text.replicate (maxLen - Text.length missingField) " ")
  <> dullyellow (text missingField)
  <+> text arrow
  <+> dullyellow (text knownField)
-}


-- INFINITE TYPES


infiniteTypeToReport :: RenderType.Localizer -> Either Hint Text -> Type.Canonical -> Report.Report
infiniteTypeToReport localizer context overallType =
  let
    (maybeRegion, description) =
      case context of
        Right name ->
          ( Nothing, functionName name )
        Left hint ->
          infiniteHint hint
  in
    Report.report
      "INFINITE TYPE"
      maybeRegion
      ( "I am inferring a weird self-referential type for " <> description <> ":"
      )
      ( stack
          [ reflowParagraph $
              "Here is my best effort at writing down the type. You will see ∞ for\
              \ parts of the type that repeat something already printed out infinitely."
          , indent 4 (RenderType.toDoc localizer overallType)
          , reflowParagraph $
              "Usually staring at the type is not so helpful in these cases, so definitely\
              \ read the debugging hints for ideas on how to figure this out: "
              <> Help.hintLink "infinite-type"
          ]
      )


infiniteHint :: Hint -> (Maybe R.Region, Text)
infiniteHint hint =
  case hint of
    CaseBranch n region ->
      ( Just region, "the " <> ordinalize n <> " `case` branch" )

    Case ->
      ( Nothing, "this `case` expression" )

    IfCondition ->
      ( Nothing, "the `if` condition" )

    IfBranches ->
      ( Nothing, "the `if` branches" )

    MultiIfBranch n region ->
      ( Just region, "the " <> ordinalize n <> " `if` branch" )

    If ->
      ( Nothing, "this `if` expression" )

    List ->
      ( Nothing, "this list" )

    ListElement n region ->
      ( Just region, "the " <> ordinalize n <> " list entry" )

    BinopLeft _ _ region ->
      ( Just region, "the left argument" )

    BinopRight _ _ region ->
      ( Just region, "the right argument" )

    Binop _ _ ->
      ( Nothing, "this expression" )

    Function maybeName ->
      ( Nothing, maybeFuncName maybeName )

    UnexpectedArg maybeName 1 1 region ->
      ( Just region, "the argument to " <> maybeFuncName maybeName )

    UnexpectedArg maybeName index _total region ->
      ( Just region, "the " <> ordinalize index <> " argument to " <> maybeFuncName maybeName )

    FunctionArity maybeName _ _ region ->
      ( Just region, maybeFuncName maybeName )

    ReturnType name _ _ region ->
      ( Just region, functionName name )

    Instance name ->
      ( Nothing, functionName name )

    Literal name ->
      ( Nothing, name )

    Pattern _ _ ->
      ( Nothing, "this pattern" )

    PatternCons ->
      ( Nothing, "this pattern" )

    Shader ->
      ( Nothing, "this shader" )

    Lambda ->
      ( Nothing, "this function" )

    Accessor _ ->
      ( Nothing, "this record accessor" )

    Access _ _ ->
      ( Nothing, "this field access" )

    Record ->
      ( Nothing, "this record" )

    Unit ->
      ( Nothing, "this “unit” value" )

    Tuple ->
      ( Nothing, "this tuple" )

    -- effect manager problems
    Manager name ->
      ( Nothing, functionName name )

    State name ->
      ( Nothing, functionName name )

    SelfMsg ->
      ( Nothing, "this code" )
