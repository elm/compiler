{-# OPTIONS_GHC -Wall #-}
module Reporting.Error.Type where

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Text.PrettyPrint.ANSI.Leijen
  ( Doc, (<>), (<+>), colon, dullyellow
  , fillSep, hang, indent, text, underline, vcat
  )

import qualified AST.Type as Type
import qualified AST.Variable as Var
import qualified Reporting.Error.Helpers as Help
import qualified Reporting.Region as Region
import qualified Reporting.Render.Type as RenderType
import qualified Reporting.Report as Report



-- ERRORS


data Error
    = Mismatch Mismatch
    | BadMain Type.Canonical
    | BadFlags Type.Canonical (Maybe String)
    | InfiniteType String Type.Canonical


data Mismatch = MismatchInfo
    { _hint :: Hint
    , _leftType :: Type.Canonical
    , _rightType :: Type.Canonical
    , _reason :: Maybe Reason
    }


data Reason
    = MessyFields [String] [String]
    | IntFloat
    | TooLongComparableTuple Int
    | BadVar (Maybe VarType) (Maybe VarType)


data VarType
    = Number
    | Comparable
    | Appendable
    | CompAppend
    | Rigid (Maybe String)


data Hint
    = CaseBranch Int Region.Region
    | Case
    | IfCondition
    | IfBranches
    | MultiIfBranch Int Region.Region
    | If
    | List
    | ListElement Int Region.Region
    | BinopLeft Var.Canonical Region.Region
    | BinopRight Var.Canonical Region.Region
    | Binop Var.Canonical
    | Function (Maybe Var.Canonical)
    | UnexpectedArg (Maybe Var.Canonical) Int Int Region.Region
    | FunctionArity (Maybe Var.Canonical) Int Int Region.Region
    | BadTypeAnnotation String
    | Instance String
    | Literal String
    | Pattern Pattern
    | Shader
    | Range
    | Lambda
    | Record
    -- effect manager problems
    | Manager String
    | State String
    | SelfMsg


data Pattern
    = PVar String
    | PAlias String
    | PData String
    | PRecord



-- TO REPORT


toReport :: RenderType.Localizer -> Error -> Report.Report
toReport localizer err =
  case err of
    Mismatch info ->
        mismatchToReport localizer info

    InfiniteType name overallType ->
        infiniteTypeToReport localizer name overallType

    BadMain tipe ->
        Report.report
          "BAD MAIN TYPE"
          Nothing
          "The `main` value has an unsupported type."
          ( Help.stack
              [ Help.reflowParagraph $
                "I need Html, Svg, or a Program so I have something to render on\
                \ screen, but you gave me:"
              , indent 4 (RenderType.toDoc localizer tipe)
              ]
          )

    BadFlags tipe maybeMessage ->
      let
        context =
          maybe "" (" the following " ++ ) maybeMessage
      in
        Report.report
          "BAD FLAGS"
          Nothing
          ("Your `main` is demanding an unsupported type as a flag."
          )
          ( Help.stack
              [ text ("The specific unsupported type is" ++ context ++ ":")
              , indent 4 (RenderType.toDoc localizer tipe)
              , text "The types of values that can flow through in and out of Elm include:"
              , indent 4 $ Help.reflowParagraph $
                  "Ints, Floats, Bools, Strings, Maybes, Lists, Arrays,\
                  \ Tuples, Json.Values, and concrete records."
              ]
          )


-- TYPE MISMATCHES


mismatchToReport :: RenderType.Localizer -> Mismatch -> Report.Report
mismatchToReport localizer (MismatchInfo hint leftType rightType maybeReason) =
  let
    report =
      Report.report "TYPE MISMATCH"

    cmpHint leftWords rightWords extraHints =
      comparisonHint localizer leftType rightType leftWords rightWords
        ( Maybe.maybeToList (reasonToString =<< maybeReason)
          ++ map toHint extraHints
        )
  in
  case hint of
    CaseBranch branchNumber region ->
        report
          (Just region)
          ( "The " ++ ordinalPair branchNumber
            ++ " branches of this `case` produce different types of values."
          )
          ( cmpHint
              ("The " ++ Help.ordinalize (branchNumber -1) ++ " branch has this type:")
              ("But the " ++ Help.ordinalize branchNumber ++ " is:")
              [ "All branches in a `case` must have the same type. So no matter\
                \ which one we take, we always get back the same type of value."
              ]
          )

    Case ->
        report
          Nothing
          ( "All the branches of this case-expression are consistent, but the overall\n"
            ++ "type does not match how it is used elsewhere."
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
          ( "The " ++ ordinalPair branchNumber
            ++ " branches of this `if` produce different types of values."
          )
          ( cmpHint
              ("The " ++ Help.ordinalize (branchNumber - 1) ++ " branch has this type:")
              ("But the "++ Help.ordinalize branchNumber ++ " is:")
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
          ("The " ++ ordinalPair elementNumber ++ " elements are different types of values.")
          ( cmpHint
              ("The " ++ Help.ordinalize (elementNumber - 1) ++ " element has this type:")
              ("But the "++ Help.ordinalize elementNumber ++ " is:")
              [ "All elements should be the same type of value so that we can\
                \ iterate through the list without running into unexpected values."
              ]
          )

    List ->
        report
          Nothing
          ( "All the elements in this list are the same type, but the overall\n"
            ++ "type does not match how it is used elsewhere."
          )
          ( cmpHint
              "The list has type:"
              "Which is fine, but the surrounding context wants it to be:"
              []
          )

    BinopLeft op region ->
        report
          (Just region)
          ("The left argument of " ++ prettyName op ++ " is causing a type mismatch.")
          ( cmpHint
              (prettyName op ++ " is expecting the left argument to be a:")
              "But the left argument is:"
              (binopHint op leftType rightType)
          )

    BinopRight op region ->
        report
          (Just region)
          ("The right argument of " ++ prettyName op ++ " is causing a type mismatch.")
          ( cmpHint
              (prettyName op ++ " is expecting the right argument to be a:")
              "But the right argument is:"
              ( binopHint op leftType rightType
                ++
                [ "I always figure out the type of the left argument first and if it is\
                  \ acceptable on its own, I assume it is \"correct\" in subsequent checks.\
                  \ So the problem may actually be in how the left and right arguments interact."
                ]
              )
          )

    Binop op ->
        report
          Nothing
          ( "The two arguments to " ++ prettyName op ++
            " are fine, but the overall type of this expression\
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
          ( "The return type of " ++ funcName maybeName ++ " is being used in unexpected ways."
          )
          ( cmpHint
              "The function results in this type of value:"
              "Which is fine, but the surrounding context wants it to be:"
              []
          )

    UnexpectedArg maybeName 1 1 region ->
        report
          (Just region)
          ("The  argument to " ++ funcName maybeName ++ " is causing a mismatch.")
          ( cmpHint
              (Help.capitalize (funcName maybeName) ++ " is expecting the argument to be:")
              "But it is:"
              (functionHint maybeName)
          )

    UnexpectedArg maybeName index _totalArgs region ->
        report
          (Just region)
          ( "The " ++ Help.ordinalize index ++ " argument to " ++ funcName maybeName
            ++ " is causing a mismatch."
          )
          ( cmpHint
              ( Help.capitalize (funcName maybeName) ++ " is expecting the "
                ++ Help.ordinalize index ++ " argument to be:"
              )
              "But it is:"
              ( functionHint maybeName
                ++
                if index == 1 then
                  []
                else
                  [ "I always figure out the type of arguments from left to right. If an argument\
                    \ is acceptable when I check it, I assume it is \"correct\" in subsequent checks.\
                    \ So the problem may actually be in how previous arguments interact with the "
                    ++ Help.ordinalize index ++ "."
                  ]
              )
          )

    FunctionArity maybeName 0 actual region ->
        let
          arg =
            if actual == 1 then "an argument" else show actual ++ " arguments"

          preHint =
            case maybeName of
              Nothing ->
                  "You are giving " ++ arg ++ " to something that is not a function!"

              Just name ->
                  prettyName name ++ " is not a function, but you are giving it " ++ arg ++ "!"
        in
          report
            (Just region)
            preHint
            (text "Maybe you forgot some parentheses? Or a comma?")

    FunctionArity maybeName expected actual region ->
        let
          s = if expected == 1 then "" else "s"
        in
          report
            (Just region)
            ( Help.capitalize (funcName maybeName) ++ " is expecting " ++ show expected
              ++ " argument" ++ s ++ ", but was given " ++ show actual ++ "."
            )
            (text "Maybe you forgot some parentheses? Or a comma?")

    BadTypeAnnotation name ->
        report
          Nothing
          ("The implementation of " ++ Help.functionName name ++ " does not match its type annotation.")
          ( cmpHint
              "The type annotation is saying:"
              "But I am inferring that the definition has this type:"
              []
          )

    Instance name ->
        report
          Nothing
          (Help.functionName name ++ " is being used in an unexpected way.")
          ( cmpHint
              ("Based on its definition, " ++ Help.functionName name ++ " has this type:")
              "But you are trying to use it as:"
              []
          )

    Literal name ->
        report
          Nothing
          ( "This " ++ name ++ " value is being used as if it is some other type of value."
          )
          ( cmpHint
              ("The " ++ name ++ " definitely has this type:")
              ("But it is being used as:")
              []
          )

    Pattern patErr ->
        let
          thing =
            case patErr of
              PVar name -> "variable `" ++ name ++ "`"
              PAlias name -> "alias `" ++ name ++ "`"
              PData name -> "tag `" ++ name ++ "`"
              PRecord -> "a record"
        in
          report
            Nothing
            ( Help.capitalize thing ++ " is causing problems in this pattern match."
            )
            ( cmpHint
                "This pattern matches things of type:"
                "But the values it will actually be trying to match are:"
                []
            )

    Shader ->
        report
          Nothing
          "There is some problem with this GLSL shader."
          ( cmpHint
              "The shader block has this type:"
              "Which is fine, but the surrounding context wants it to be:"
              []
          )

    Range ->
        report
          Nothing
          "The low and high members of this list range are not the same type of value."
          ( cmpHint
              "The low end of the range has type:"
              "But the high end is:"
              []
          )

    Lambda ->
        report
          Nothing
          "This anonymous function is being used in an unexpected way."
          ( cmpHint
              "The anonymous function has type:"
              "But you are trying to use it as:"
              []
          )

    Record ->
        report
          Nothing
          "This record is being used in an unexpected way."
          ( cmpHint
              "The record has type:"
              "But you are trying to use it as:"
              []
          )

    Manager name ->
        report
          Nothing
          ("The `" ++ name ++ "` in your effect manager has a weird type.")
          ( cmpHint
              ("Your `" ++ name ++ "` function has this type:")
              "But it needs to have a type like this:"
              [ "You can read more about setting up effect managers properly here:\
                \ <http://guide.elm-lang.org/effect_managers/>"
              ]
          )

    State name ->
        report
          Nothing
          ( "Your effect manager creates a certain type of state with `init`, but your `"
            ++ name ++ "` function expects a different kind of state."
          )
          ( cmpHint
              "The state created by `init` has this type:"
              ("But `" ++ name ++ "` expects state of this type:")
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




comparisonHint
    :: RenderType.Localizer
    -> Type.Canonical
    -> Type.Canonical
    -> String
    -> String
    -> [Doc]
    -> Doc
comparisonHint localizer leftType rightType leftWords rightWords finalHints =
  let
    (leftDoc, rightDoc) =
      RenderType.diffToDocs localizer leftType rightType
  in
    Help.stack $
      [ Help.reflowParagraph leftWords
      , indent 4 leftDoc
      , Help.reflowParagraph rightWords
      , indent 4 rightDoc
      ]
      ++
      finalHints



-- BINOP HINTS


binopHint :: Var.Canonical -> Type.Canonical -> Type.Canonical -> [String]
binopHint op leftType rightType =
  let
    leftString =
      show (RenderType.toDoc Map.empty leftType)

    rightString =
      show (RenderType.toDoc Map.empty rightType)
  in
    if Var.is ["Basics"] "+" op && elem "String" [leftString, rightString] then
        [ "To append strings in Elm, you need to use the (++) operator, not (+). "
          ++ "<http://package.elm-lang.org/packages/elm-lang/core/latest/Basics#++>"
        ]

    else if Var.is ["Basics"] "/" op && elem "Int" [leftString, rightString] then
        [ "The (/) operator is specifically for floating point division, and (//) is\
          \ for integer division. You may need to do some conversions between ints and\
          \ floats to get both arguments matching the division operator you want."
        ]

    else
        []



-- FUNCTION HINTS


functionHint :: Maybe Var.Canonical -> [String]
functionHint maybeName =
  case maybeName of
    Nothing ->
      []

    Just name ->
      if Var.inHtml ["Html","App"] "program" == name then
        [ "Does your program have flags? Maybe you want `programWithFlags` instead."
        ]

      else
        []



-- MISMATCH HELPERS


ordinalPair :: Int -> String
ordinalPair number =
  Help.ordinalize (number -1 ) ++ " and " ++ Help.ordinalize number


prettyName :: Var.Canonical -> String
prettyName (Var.Canonical _ opName) =
  Help.functionName opName


funcName :: Maybe Var.Canonical -> String
funcName maybeVar =
  case maybeVar of
    Nothing ->
      "this function"

    Just var ->
      "function " ++ prettyName var



-- MISMTACH REASONS


flipReason :: Reason -> Reason
flipReason reason =
  case reason of
    MessyFields leftOnly rightOnly ->
        MessyFields rightOnly leftOnly

    IntFloat ->
        IntFloat

    TooLongComparableTuple len ->
        TooLongComparableTuple len

    BadVar left right ->
        BadVar right left


reasonToString :: Reason -> Maybe Doc
reasonToString reason =
  let
    go msg =
      Just (toHint msg)
  in
  case reason of
    MessyFields leftOnly rightOnly ->
        do  let typos = Help.findPotentialTypos leftOnly rightOnly
            _ <- Help.vetTypos typos
            misspellingMessage typos

    IntFloat ->
        go
          "Elm does not automatically convert between Ints and Floats. Use\
          \ `toFloat` and `round` to do specific conversions.\
          \ <http://package.elm-lang.org/packages/elm-lang/core/latest/Basics#toFloat>"

    TooLongComparableTuple len ->
        go $
          "Although tuples are comparable, this is currently only supported\
          \ for tuples with 6 or fewer entries, not " ++ show len ++ "."

    BadVar (Just Comparable) _ ->
        go "Only ints, floats, chars, strings, lists, and tuples are comparable."

    BadVar (Just Appendable) _ ->
        go "Only strings, text, and lists are appendable."

    BadVar (Just CompAppend) _ ->
        go "Only strings and lists are both comparable and appendable."

    BadVar (Just (Rigid _)) (Just (Rigid _)) ->
        go doubleRigidError

    BadVar (Just (Rigid _)) _  ->
        go singleRigidError

    BadVar _ (Just (Rigid _))  ->
        go singleRigidError

    BadVar _ _ ->
        Nothing


singleRigidError :: String
singleRigidError =
  "A type annotation is too generic. You can probably just switch to the\
  \ type I inferred. These issues can be subtle though, so read more about it. "
  ++ Help.hintLink "type-annotations"


doubleRigidError :: String
doubleRigidError =
  "A type annotation is clashing with itself or with a sub-annotation.\
  \ This can be particularly tricky, so read more about it. "
  ++ Help.hintLink "type-annotations"


hintDoc :: Doc
hintDoc =
  underline (text "Hint") <> colon


toHint :: String -> Doc
toHint str =
  fillSep (hintDoc : map text (words str))


misspellingMessage :: [(String,String)] -> Maybe Doc
misspellingMessage typos =
  if null typos then
      Nothing

  else
      let
        maxLen =
          maximum (map (length . fst) typos)
      in
        Just $ hang 4 $ vcat $
          toHint "I compared the record fields and found some potential typos."
          : text ""
          : map (pad maxLen) typos


pad :: Int -> (String, String) -> Doc
pad maxLen (leftField, rightField) =
  text (replicate (maxLen - length leftField) ' ')
  <> dullyellow (text leftField)
  <+> text "<->"
  <+> dullyellow (text rightField)



-- INFINITE TYPES


infiniteTypeToReport
    :: RenderType.Localizer
    -> String
    -> Type.Canonical
    -> Report.Report
infiniteTypeToReport localizer name overallType =
  Report.report
    "INFINITE TYPE"
    Nothing
    ( "I am inferring a weird self-referential type for " ++ Help.functionName name
    )
    ( Help.stack
        [ Help.reflowParagraph $
            "Here is my best effort at writing down the type. You will see ? and ∞ for\
            \ parts of the type that repeat something already printed out infinitely."
        , indent 4 (RenderType.toDoc localizer overallType)
        , Help.reflowParagraph $
            "Usually staring at the type is not so helpful in these cases, so definitely\
            \ read the debugging hints for ideas on how to figure this out: "
            ++ Help.hintLink "infinite-type"
        ]
    )

