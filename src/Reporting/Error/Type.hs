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
          (subRegion, preHint) = hintToString hint

          postHint =
            maybe "" (++"\n\n") note
            ++ "As I infer the type of values flowing through your program, I see a conflict\n"
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


hintToString :: Hint -> (Maybe Region.Region, String)
hintToString hint =
  case hint of
    CaseBranch branchNumber region ->
        ( Just region
        , "The branches of this case-expression return different types of values.\n\n"
          ++ "I noticed the mismatch in the " ++ ordinalize branchNumber ++ " branch, but go through and make sure every\n"
          ++ "branch returns the same type of value."
        )

    Case ->
        ( Nothing
        , "All the branches of this case-expression are consistent, but the overall\n"
          ++ "type does not match how it is used elsewhere."
        )

    IfBranches ->
        ( Nothing
        , "The branches of this if-expression return different types of values.\n"
          ++ "All branches must have the same return type!"
        )

    MultiIfBranch branchNumber region ->
        ( Just region
        , "The branches of this if-expression return different types of values.\n\n"
          ++ "I noticed the mismatch in the " ++ ordinalize branchNumber ++ " branch, but go through and make sure every\n"
          ++ "branch returns the same type of value."
        )

    If ->
        ( Nothing
        , "All the branches of this if-expression are consistent, but the overall\n"
          ++ "type does not match how it is used elsewhere."
        )

    ListElement elementNumber region ->
        ( Just region
        , "Not all elements of this list are the same type of value.\n\n"
          ++ "I noticed the mismatch in the " ++ ordinalize elementNumber ++ " element, but go through and make sure every\n"
          ++ "element is the same type of value."
        )

    List ->
        ( Nothing
        , "All the elements in this list are the same type, but the overall\n"
          ++ "type does not match how it is used elsewhere."
        )

    BinopLeft op region ->
        ( Just region
        , "The left argument of " ++ prettyOperator op ++ " is causing a type mismatch."
        )

    BinopRight op region ->
        ( Just region
        , "The right argument of " ++ prettyOperator op ++ " is causing a type mismatch."
        )

    Binop op ->
        ( Nothing
        , "The two arguments to " ++ prettyOperator op ++ " are fine, but the overall type of this expression\n"
          ++ "does not match how it is used elsewhere."
        )

    Function maybeName ->
        ( Nothing
        , "The return type of " ++ funcName maybeName ++ " is being used in unexpected ways."
        )

    UnexpectedArg maybeName index region ->
        ( Just region
        , "The " ++ ordinalize index ++ " argument to " ++ funcName maybeName
          ++ " has an unexpected type."
        )

    FunctionArity maybeName expected actual region ->
        let
          s = if expected <= 1 then "" else "s"
        in
          ( Just region
          , capitalize (funcName maybeName) ++ " is expecting " ++ show expected
            ++ " argument" ++ s ++ ", but was given " ++ show actual ++ "."
          )

    BadTypeAnnotation name ->
        ( Nothing
        , "The type annotation for `" ++ name ++ "` does not match its definition."
        )

    Instance name ->
        ( Nothing
        , "Given how `" ++ name ++ "` is defined, this use will not work out."
        )

    Literal name ->
        ( Nothing
        , "This " ++ name ++ " value is being used as if it is some other type of value."
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
          )

    Shader ->
        ( Nothing
        , "There is some problem with this GLSL shader."
        )

    Range ->
        ( Nothing
        , "The low and high members of this list range do not match."
        )

    Lambda ->
        ( Nothing
        , "This anonymous function is being used in an unexpected way."
        )

    Record ->
        ( Nothing
        , "This record is being used in an unexpected way."
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
