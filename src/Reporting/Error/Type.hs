{-# OPTIONS_GHC -Wall #-}
module Reporting.Error.Type where

import qualified Data.Char as Char
import qualified Data.List as List

import qualified AST.Helpers as AstHelp
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
    | NotRecord
    | TooLongComparableTuple Int
    | Rigid (Maybe String)
    deriving (Show)


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


toReport :: RenderType.Localizer -> Error -> Report.Report
toReport localizer err =
  case err of
    Mismatch info ->
        mismatchToReport localizer info

    InfiniteType name var tipe ->
        let
          prettyVar =
            error "TODO P.pretty localizer False var"

          prettyType =
            error "TODO P.pretty localizer False tipe"
        in
        Report.simple "INFINITE TYPE"
          ( "I am inferring weird self-referential type for `" ++ name ++ "`"
          )
          ( "TODO - do a better job\n\n"
            ++ "The bit of the type that is self-referential looks like this:\n\n"
            ++ error "TODO P.render (P.nest 4 (prettyVar <+> P.equals <+> prettyType))"
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
          ++ RenderType.tipe localizer tipe


mismatchToReport :: RenderType.Localizer -> Mismatch -> Report.Report
mismatchToReport localizer (MismatchInfo hint leftType rightType maybeReason) =
  let
    report =
      Report.Report "TYPE MISMATCH"

    pretty tipe =
      "\n\n"
      ++ RenderType.tipe localizer tipe
      ++ "\n\n"
  in
  case hint of
    CaseBranch branchNumber region ->
        report
          (Just region)
          ( "The " ++ ordinalize (branchNumber -1) ++ " and " ++ ordinalize branchNumber
            ++ " branches of this `case` result in different types of values."
          )
          ( "The " ++ ordinalize (branchNumber -1) ++ " branch results in this type of value:\n"

            ++ pretty leftType

            ++ "And the " ++ ordinalize branchNumber ++ " branch results in this type:\n"

            ++ pretty rightType

            ++ "These branches should match so that no matter which one we take, we always get\n"
            ++ "back the same type of value."
          )

    Case ->
        report
          Nothing
          ( "All the branches of this case-expression are consistent, but the overall\n"
            ++ "type does not match how it is used elsewhere."
          )
          ( error "TODO generic error"
          )

    IfCondition ->
        report
          Nothing
          ( "This condition does not evaluate to a boolean value, True or False."
          )
          ( "Instead of Bool, it is resulting in:\n"

            ++ pretty rightType

            ++ "Note: Elm does not have \"truthiness\" such that ints and strings and lists\n"
            ++ "are automatically converted to booleans. Do that conversion explicitly."
          )

    IfBranches ->
        report
          Nothing
          ( "The branches of this `if` result in different types of values."
          )
          ( "The `then` branch results in:\n"

            ++ pretty leftType

            ++ "And the `else` branch results in:\n"

            ++ pretty rightType

            ++ "These need to match so that no matter which one we take, we always get\n"
            ++ "back the same type of value."
          )

    MultiIfBranch branchNumber region ->
        report
          (Just region)
          ( "The " ++ ordinalize branchNumber ++ " branch of this `if` results in an unexpected type of value."
          )
          ( "The " ++ ordinalize (branchNumber - 1) ++ " branch results in:\n"

            ++ pretty leftType

            ++ "And the "++ ordinalize branchNumber ++ " branch results in:\n"

            ++ pretty rightType

            ++ "All branches should match so that no matter which one we take, we always get\n"
            ++ "back the same type of value."
          )

    If ->
        report
          Nothing
          ( "All the branches of this if-expression are consistent, but the overall\n"
            ++ "type does not match how it is used elsewhere."
          )
          ( "The `if` evaluates to something of type:\n"

            ++ pretty leftType

            ++ "Which is fine, but the surrounding context wants it to be:\n"

            ++ pretty rightType

          )

    ListElement elementNumber region ->
        report
          (Just region)
          ( "The " ++ ordinalize elementNumber ++ " element of this list is an unexpected type of value."
          )
          ( "All elements should be the same type of value so that we can iterate over the\nlist without running into unexpected values."
          )

    List ->
        report
          Nothing
          ( "All the elements in this list are the same type, but the overall\n"
            ++ "type does not match how it is used elsewhere."
          )
          ( "The list has type:\n"

            ++ pretty leftType

            ++ "Which is fine, but the surrounding context wants it to be:\n"

            ++ pretty rightType
          )

    BinopLeft op region ->
        report
          (Just region)
          ("The left argument of " ++ prettyOperator op ++ " is causing a type mismatch.")
          ( prettyOperator op ++ " is expecting:"

            ++ pretty leftType

            ++ "But the value on the left has this type:"

            ++ pretty rightType
          )

    BinopRight op region ->
        report
          (Just region)
          ( "The right argument of " ++ prettyOperator op ++ " is causing a type mismatch."
          )
          ( prettyOperator op ++ " is expecting:"

            ++ pretty leftType

            ++ "But the value on the right has this type:"

            ++ pretty rightType
          )

    Binop op ->
        report
          Nothing
          ( "The two arguments to " ++ prettyOperator op ++ " are fine, but the overall type of this expression\n"
            ++ "does not match how it is used elsewhere."
          )
          ( "The result of this binary operation is:\n"

            ++ pretty leftType

            ++ "Which is fine, but the surrounding context wants it to be:\n"

            ++ pretty rightType

          )

    Function maybeName ->
        report
          Nothing
          ( "The return type of " ++ funcName maybeName ++ " is being used in unexpected ways."
          )
          ( "The function results in a value with type:\n"

            ++ pretty leftType

            ++ "Which is fine, but the surrounding context wants it to be:\n"

            ++ pretty rightType
          )

    UnexpectedArg maybeName index region ->
        report
          (Just region)
          ( "The " ++ ordinalize index ++ " argument to " ++ funcName maybeName
            ++ " is causing a mismatch."
          )
          ( "The " ++ ordinalize index ++ " to " ++ funcName maybeName ++ " should be:"

            ++ pretty leftType

            ++ "But it is:"

            ++ pretty rightType

            ++ maybe "" reasonToString maybeReason
          )

    FunctionArity maybeName expected actual region ->
        let
          s = if expected <= 1 then "" else "s"
        in
          report
            (Just region)
            ( capitalize (funcName maybeName) ++ " is expecting " ++ show expected
              ++ " argument" ++ s ++ ", but was given " ++ show actual ++ "."
            )
            ( error "TODO are the types interesting here?"
            )

    BadTypeAnnotation name ->
        report
          Nothing
          ( "The type annotation for `" ++ name ++ "` does not match its definition."
          )
          ( "Based on the actual definition, the type should be:\n"

            ++ pretty rightType
          )

    Instance name ->
        report
          Nothing
          ( "This usage of `" ++ name ++ "` is causing a type mismatch."
          )
          ( "Based on its definition, `" ++ name ++ "` has this type:"

            ++ pretty leftType

            ++ "But you are trying to use it as:"

            ++ pretty rightType
          )

    Literal name ->
        report
          Nothing
          ( "This " ++ name ++ " value is being used as if it is some other type of value."
          )
          ( "You are trying to use this " ++ name ++ " as this type:"

            ++ pretty rightType
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
          report
            Nothing
            ( "Problem with " ++ thing ++ " in this pattern match."
            )
            ( "This pattern matches things of type:"

              ++ pretty leftType

              ++ "But the values it will actually be trying to match are:"

              ++ pretty rightType
            )

    Shader ->
        report
          Nothing
          ( "There is some problem with this GLSL shader."
          )
          ( error "TODO generic error"
          )

    Range ->
        report
          Nothing
          ( "The low and high members of this list range are not the same type of value."
          )
          ( "The low end of the range has type:"

            ++ pretty leftType

            ++ "But the high end is:"

            ++ pretty rightType
          )

    Lambda ->
        report
          Nothing
          ( "This anonymous function is being used in an unexpected way."
          )
          ( "The anonymous function has type:"

            ++ pretty leftType

            ++ "But you are trying to use it as:"

            ++ pretty rightType
          )

    Record ->
        report
          Nothing
          ( "This record is being used in an unexpected way."
          )
          ( "The record has type:"

            ++ pretty leftType

            ++ "But you are trying to use it as:"

            ++ pretty rightType
          )


reasonToString :: Reason -> String
reasonToString reason =
  case reason of
    MessyFields expecteds [] ->
        genericMissingFieldsMessage "former" expecteds

    MessyFields [] actuals ->
        genericMissingFieldsMessage "latter" actuals

    MessyFields expecteds actuals ->
        case concatMap (similarNames actuals) expecteds of
          [] ->
              "These records look pretty different."

          [(exp,act)] ->
              "Looks like a misspelling between field " ++ exp ++ " and " ++ act ++ "."

          potentialTypos ->
              "There are some potential typos in the record field names.\n"
              ++ concatMap (\(exp,act) -> "\n    " ++ exp ++ " <=> " ++ act) potentialTypos

    NotRecord ->
        "A record cannot be merged with other types of values."

    TooLongComparableTuple len ->
        error "TODO"

    Rigid maybeName ->
        error "TODO"


similarNames :: [String] -> String -> [(String,String)]
similarNames otherKeys key =
  map ((,) key) (filter (\key' -> Help.distance key key' == 1) otherKeys)



genericMissingFieldsMessage :: String -> [String] -> String
genericMissingFieldsMessage latterOrFormer fields =
  "A record in the " ++ latterOrFormer ++ " type is missing "
  ++
  case fields of
    [key] ->
      "field `" ++ key ++ "`"

    [key1,key2] ->
      "fields " ++ key1 ++ " and " ++ key2

    keys ->
      "fields " ++ List.intercalate ", " (init keys) ++ ", and " ++ last keys


prettyOperator :: Var.Canonical -> String
prettyOperator (Var.Canonical _ opName) =
  if AstHelp.isOp opName then
      "(" ++ opName ++ ")"

  else
      "`" ++ opName ++ "`"


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


