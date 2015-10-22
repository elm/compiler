{-# OPTIONS_GHC -Wall #-}
module Reporting.Error.Type where

import qualified Data.Maybe as Maybe
import Text.PrettyPrint.ANSI.Leijen
  ( Doc, (<>), (<+>), colon, dullyellow, equals
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
    | InfiniteType
        { _name :: String
        , _overallType :: Type.Canonical
        , _infiniteType :: Type.Canonical
        }


data Mismatch = MismatchInfo
    { _hint :: Hint
    , _leftType :: Type.Canonical
    , _rightType :: Type.Canonical
    , _reason :: Maybe Reason
    }


data Reason
    = MessyFields [String] [String]
    | NotNumber
    | NotComparable
    | NotAppendable
    | NotCompAppend
    | IntFloat
    | TooLongComparableTuple Int
    | Rigid (Maybe String)
    | DoubleRigid


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

    InfiniteType name overallType infiniteType ->
        infiniteTypeToReport localizer name overallType infiniteType

    BadMain tipe ->
        Report.report
          "BAD MAIN TYPE"
          Nothing
          "The 'main' value has an unsupported type."
          ( Help.stack
              [ Help.reflowParagraph $
                "I need an Element, Html, (Signal Element), or (Signal Html) so I can render it\
                \ on screen, but you gave me:"
              , indent 4 (RenderType.toDoc localizer tipe)
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
        ( map toHint extraHints
          ++ Maybe.maybeToList (reasonToString =<< maybeReason)
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
              "You have given me an condition with this type:"
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
              []
          )

    BinopRight op region ->
        report
          (Just region)
          ("The right argument of " ++ prettyName op ++ " is causing a type mismatch.")
          ( cmpHint
              (prettyName op ++ " is expecting the right argument to be a:")
              "But the right argument is:"
              []
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
              []
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
              []
          )

    FunctionArity maybeName expected actual region ->
        let
          s = if expected <= 1 then "" else "s"
        in
          report
            (Just region)
            ( Help.capitalize (funcName maybeName) ++ " is expecting " ++ show expected
              ++ " argument" ++ s ++ ", but was given " ++ show actual ++ "."
            )
            (text "Maybe you forgot some parentheses somewhere?")

    BadTypeAnnotation name ->
        report
          Nothing
          ("The type annotation for " ++ Help.functionName name ++ " does not match its definition.")
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


reasonToString :: Reason -> Maybe Doc
reasonToString reason =
  let
    go msg =
      Just (toHint msg)
  in
  case reason of
    MessyFields leftOnly rightOnly ->
        do  let typos = RenderType.findPotentialTypos leftOnly rightOnly
            _ <- RenderType.vetTypos typos
            misspellingMessage typos

    NotNumber ->
        go "Valid number types include Int and Float."

    NotComparable ->
        go "Valid comparable types include Int, Float, Char, String, lists, and tuples."

    NotAppendable ->
        go "Valid appendable types include String, Text, and List."

    NotCompAppend ->
        go "Valid `number` types include Int and Float."

    IntFloat ->
        go
          "Elm does not automatically convert between Ints and Floats. Use\
          \ `toFloat` and `round` to do specific conversions.\
          \ <http://package.elm-lang.org/packages/elm-lang/core/latest/Basics#toFloat>"

    TooLongComparableTuple len ->
        go $
          "Although tuples are comparable, this is currently only supported\
          \ for tuples with 6 or fewer entries, not " ++ show len ++ "."

    Rigid (Just name) ->
        go $
          "It is very likely that a type annotation is too generic. Type variable\
          \ `" ++ name ++ "` is saying *many* kinds of value can be given, but the\
          \ actual definition needs a more specific type. I am not sure whether the\
          \ type annotation or the definition is \"right\" though, you may have to\
          \ make the annotation more specific or make the definition more generic."

    Rigid Nothing ->
        go $
          "It is very likely that a type annotation is too generic. One type is saying\
          \ *many* kinds of value can be given, but the other needs a more specific type."

    DoubleRigid ->
        Just $ Help.stack $
          [ hintDoc <> text "This usually happens when a few factors come together."
          , indent 4 $ vcat $
              [ text "1. You have a generic function with a type annotation."
              , text "2. In its definition, there is a `let` with generic type annotations."
              ]
          , Help.reflowParagraph
              "The issue is that these type variables are not actually shared between\
              \ outer and inner type annotations right now. You can usually fix this by\
              \ commenting out the inner type annotations."
          ]



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
    -> Type.Canonical
    -> Report.Report
infiniteTypeToReport localizer name overallType infiniteType =
  Report.report
    "INFINITE TYPE"
    Nothing
    ( "I am inferring a weird self-referential type for " ++ Help.functionName name
    )
    ( Help.stack $
        explainInfiniteType localizer name overallType infiniteType
        ++
        [ text $
            "This can be very tricky to figure out, so definitely read the debugging hints:\n"
            ++ Help.hintLink "infinite-type"
        ]
    )


explainInfiniteType
    :: RenderType.Localizer
    -> String
    -> Type.Canonical
    -> Type.Canonical
    -> [Doc]
explainInfiniteType localizer name overallType infiniteType =
  let
    selfReference =
      text "?" <+> equals <+> RenderType.toDoc localizer infiniteType
  in
    case overallType of
      Type.Var "?" ->
          [ Help.reflowParagraph $
              "I cannot write down the actual type of " ++ Help.functionName name ++
              " because it is infinitely large (that's the problem!) so here it\
              \ is as an equation where `?` represents the type:"
          , indent 4 selfReference
          ]

      _ ->
          [ Help.reflowParagraph $
              "I am inferring that " ++ Help.functionName name ++ " has this type:"
          , indent 4 (RenderType.toDoc localizer overallType)
          , Help.reflowParagraph $
              "The problem is in the `?` which seems to contain itself. I cannot show\
              \ you that type directly (because it is infinitely large!) so here it is\
              \ as an equation:"
          , indent 4 selfReference
          ]
