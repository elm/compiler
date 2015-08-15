{-# OPTIONS_GHC -Wall #-}
module Reporting.Error.Type where

import qualified Data.Char as Char
import Text.PrettyPrint ((<+>))
import qualified Text.PrettyPrint as P

import qualified AST.Helpers as Help
import qualified AST.Type as Type
import qualified AST.Variable as Var
import qualified Reporting.PrettyPrint as P
import qualified Reporting.Region as Region
import qualified Reporting.Report as Report


data Error
    = Mismatch Mismatch
    | InfiniteType InfiniteType
    | BadMain Type.Canonical


data Mismatch = MismatchInfo
    { _hint :: Hint
    , _leftType :: Type.Canonical
    , _rightType :: Type.Canonical
    , _note :: Maybe String
    }


data InfiniteType = InfiniteTypeInfo
    { _name :: String
    , _var :: Type.Canonical
    , _type :: Type.Canonical
    }


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
    | UnexpectedArg (Maybe Var.Canonical) Int Region.Region
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

toReport :: P.Dealiaser -> Error -> Report.Report
toReport dealiaser err =
  case err of
    Mismatch (MismatchInfo hint leftType rightType note) ->
        let
          (subRegion, preHint, maybePostHint) = hintToString hint

          postHint =
            maybe "" (++"\n\n") note
            ++ maybe "" (++"\n\n") maybePostHint
            ++ "As I infer the types of values flowing through your program, I see a conflict\n"
            ++ "between these two types:\n\n"
            ++ P.render (P.nest 4 (P.pretty dealiaser False leftType))
            ++ "\n\n"
            ++ P.render (P.nest 4 (P.pretty dealiaser False rightType))
        in
          Report.Report "TYPE MISMATCH" subRegion preHint postHint

    InfiniteType (InfiniteTypeInfo name var tipe) ->
        let
          prettyVar =
            P.pretty dealiaser False var

          prettyType =
            P.pretty dealiaser False tipe
        in
        Report.simple "INFINITE TYPE"
          ( "I am inferring weird self-referential type for `" ++ name ++ "`"
          )
          ( "The bit of the type that is self-referential looks like this:\n\n"
            ++ P.render (P.nest 4 (prettyVar <+> P.equals <+> prettyType))
            ++ "\n\nThe cause is often that the usage of `" ++ name ++ "` is flipped around.\n\n"
            ++ "Maybe you are inserting a data structure into an element? Maybe you are giving\n"
            ++ "a function to an argument? Either way, something is probably backwards!\n\n"
            ++ "Try breaking the code related to `" ++ name ++ "` into smaller pieces.\n"
            ++ "Give each piece a name and try to write down its type."
          )

    BadMain tipe ->
        Report.simple "BAD MAIN TYPE" "The 'main' value has an unsupported type." $
          "I need an Element, Html, (Signal Element), or (Signal Html) so I can render it\n"
          ++ "on screen, but you gave me:\n\n"
          ++ P.render (P.nest 4 (P.pretty dealiaser False tipe))


hintToString :: Hint -> (Maybe Region.Region, String, Maybe String)
hintToString hint =
  case hint of
    CaseBranch branchNumber region ->
        ( Just region
        , "The " ++ ordinalize branchNumber ++ " branch of this `case` results in an unexpected type of value."
        , Just "All branches should match so that no matter which one we take, we always get\nback the same type of value."
        )

    Case ->
        ( Nothing
        , "All the branches of this case-expression are consistent, but the overall\n"
          ++ "type does not match how it is used elsewhere."
        , Nothing
        )

    IfCondition ->
        ( Nothing
        , "This condition does not evaluate to a boolean value, True or False."
        , Just "Elm does not have \"truthiness\" such that ints and strings and lists are\nautomatically converted to booleans. Do that conversion explicitly."
        )

    IfBranches ->
        ( Nothing
        , "The branches of this `if` result in different types of values."
        , Just "All branches should match so that no matter which one we take, we always get\nback the same type of value."
        )

    MultiIfBranch branchNumber region ->
        ( Just region
        , "The " ++ ordinalize branchNumber ++ " branch of this `if` results in an unexpected type of value."
        , Just "All branches should match so that no matter which one we take, we always get\nback the same type of value."
        )

    If ->
        ( Nothing
        , "All the branches of this if-expression are consistent, but the overall\n"
          ++ "type does not match how it is used elsewhere."
        , Nothing
        )

    ListElement elementNumber region ->
        ( Just region
        , "The " ++ ordinalize elementNumber ++ " element of this list is an unexpected type of value."
        , Just "All elements should be the same type of value so that we can iterate over the\nlist without running into unexpected values."
        )

    List ->
        ( Nothing
        , "All the elements in this list are the same type, but the overall\n"
          ++ "type does not match how it is used elsewhere."
        , Nothing
        )

    BinopLeft op region ->
        ( Just region
        , "The left argument of " ++ prettyOperator op ++ " is causing a type mismatch."
        , Nothing
        )

    BinopRight op region ->
        ( Just region
        , "The right argument of " ++ prettyOperator op ++ " is causing a type mismatch."
        , Nothing
        )

    Binop op ->
        ( Nothing
        , "The two arguments to " ++ prettyOperator op ++ " are fine, but the overall type of this expression\n"
          ++ "does not match how it is used elsewhere."
        , Nothing
        )

    Function maybeName ->
        ( Nothing
        , "The return type of " ++ funcName maybeName ++ " is being used in unexpected ways."
        , Nothing
        )

    UnexpectedArg maybeName index region ->
        ( Just region
        , "The " ++ ordinalize index ++ " argument to " ++ funcName maybeName
          ++ " has an unexpected type."
        , Nothing
        )

    FunctionArity maybeName expected actual region ->
        let
          s = if expected <= 1 then "" else "s"
        in
          ( Just region
          , capitalize (funcName maybeName) ++ " is expecting " ++ show expected
            ++ " argument" ++ s ++ ", but was given " ++ show actual ++ "."
          , Nothing
          )

    BadTypeAnnotation name ->
        ( Nothing
        , "The type annotation for `" ++ name ++ "` does not match its definition."
        , Nothing
        )

    Instance name ->
        ( Nothing
        , "Given how `" ++ name ++ "` is defined, this use will not work out."
        , Nothing
        )

    Literal name ->
        ( Nothing
        , "This " ++ name ++ " value is being used as if it is some other type of value."
        , Nothing
        )

    Pattern patErr ->
        let
          thing =
            case patErr of
              PVar name -> "variable `" ++ name ++ "`"
              PAlias name -> "alias `" ++ name ++ "`"
              PData name -> "`" ++ name ++ "`"
              PRecord -> "a record"
        in
          ( Nothing
          , "Problem with " ++ thing ++ " in this pattern match."
          , Nothing
          )

    Shader ->
        ( Nothing
        , "There is some problem with this GLSL shader."
        , Nothing
        )

    Range ->
        ( Nothing
        , "The low and high members of this list range are not the same type of value."
        , Nothing
        )

    Lambda ->
        ( Nothing
        , "This anonymous function is being used in an unexpected way."
        , Nothing
        )

    Record ->
        ( Nothing
        , "This record is being used in an unexpected way."
        , Nothing
        )


prettyOperator :: Var.Canonical -> String
prettyOperator (Var.Canonical _ opName) =
  if Help.isOp opName
    then "(" ++ opName ++ ")"
    else "`" ++ opName ++ "`"


funcName :: Maybe Var.Canonical -> String
funcName maybeVar =
  case maybeVar of
    Nothing ->
      "this function"

    Just var ->
      "function " ++ prettyOperator var


capitalize :: String -> String
capitalize string =
  case string of
    [] -> []
    c : cs ->
      Char.toUpper c : cs


ordinalize :: Int -> String
ordinalize number =
  let
    remainder10 =
      number `mod` 10

    remainder100 =
      number `mod` 100

    ending
      | remainder100 `elem` [11..13] = "th"
      | remainder10 == 1             = "st"
      | remainder10 == 2             = "nd"
      | remainder10 == 3             = "rd"
      | otherwise                    = "th"
  in
    show number ++ ending
