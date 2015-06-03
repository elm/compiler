{-# OPTIONS_GHC -Wall #-}
module Reporting.Error.Type where

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
    , _note :: Note
    }


data InfiniteType = InfiniteTypeInfo
    { _name :: String
    , _var :: Type.Canonical
    , _type :: Type.Canonical
    }


data Hint
    = None
    | CaseBranch Int Region.Region
    | Case
    | IfBranches
    | MultiIfBranch Int Region.Region
    | If
    | List
    | ListElement Int Region.Region
    | BinopLeft Var.Canonical Region.Region
    | BinopRight Var.Canonical Region.Region
    | Binop Var.Canonical
    | BadArgument Region.Region
    | ExtraArgument Region.Region
    | BadTypeAnnotation String


data Note
    = PreNote String
    | NoNote
    | PostNote String


-- TO REPORT

toReport :: P.Dealiaser -> Error -> Report.Report
toReport dealiaser err =
  case err of
    Mismatch (MismatchInfo hint leftType rightType note) ->
        let
          (subRegion, preHint) = hintToString hint

          postHint =
            "To be more specific, type inference is leading to a conflict between this type:\n\n"
            ++ P.render (P.nest 4 (P.pretty dealiaser False leftType))
            ++ "\n\nand this type:\n\n"
            ++ P.render (P.nest 4 (P.pretty dealiaser False rightType))
        in
          case note of
            PreNote msg ->
              Report.Report "TYPE MISMATCH" subRegion (preHint ++ "\n\n" ++ msg) postHint

            NoNote ->
              Report.Report "TYPE MISMATCH" subRegion preHint postHint

            PostNote msg ->
              Report.Report "TYPE MISMATCH" subRegion preHint (postHint ++ "\n\n" ++ msg)

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
    None ->
        ( Nothing
        , "This expression is triggering a type mismatch."
        )

    CaseBranch branchNumber region ->
        ( Just region
        , "The branches of this case-expression return different types of values.\n\n"
          ++ "I noticed the mismatch in branch #" ++ show branchNumber ++ ", but go through and make sure every\n"
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
          ++ "I noticed the mismatch in branch #" ++ show branchNumber ++ ", but go through and make sure every\n"
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
          ++ "I noticed the mismatch in element #" ++ show elementNumber ++ ", but go through and make sure every\n"
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

    BadArgument region ->
        ( Just region
        , "This argument is causing a type mismatch."
        )

    ExtraArgument region ->
        ( Just region
        , "This expression is mistakenly being used as a function.\n"
          ++ "Maybe you provided an extra argument?"
        )

    BadTypeAnnotation name ->
        ( Nothing
        , "The type annotation for `" ++ name ++ "` does not match its definition."
        )


prettyOperator :: Var.Canonical -> String
prettyOperator (Var.Canonical _ opName) =
  if Help.isOp opName
    then "(" ++ opName ++ ")"
    else "`" ++ opName ++ "`"
