{-# OPTIONS_GHC -Wall #-}
module Reporting.Error.Type where

import qualified Text.PrettyPrint as P

import qualified AST.Type as T
import qualified Reporting.PrettyPrint as P
import qualified Reporting.Region as Region
import qualified Reporting.Report as Report


data Error
    = Mismatch (Mismatch Hint)
    | InfiniteType String T.Canonical
    | BadMain T.Canonical


data Mismatch a = MismatchInfo
    { _hint :: a
    , _leftType :: T.Canonical
    , _rightType :: T.Canonical
    }


data Hint
    = None
    | Custom String
    | CaseBranch Int Region.Region
    | Case
    | IfBranches
    | MultiIfBranch Int Region.Region
    | If


mergeHint :: Hint -> Maybe String -> Hint
mergeHint hint maybeMessage =
  let
    promote message =
      case hint of
        None -> Custom message
        _ -> hint
  in
    maybe hint promote maybeMessage


-- TO REPORT

toReport :: Error -> Report.Report
toReport err =
  case err of
    Mismatch (MismatchInfo hint leftType rightType) ->
        let
          (subRegion, preHint) = hintToString hint
        in
            Report.Report subRegion preHint $
              "During type inference, I am seeing a conflict between this type:\n\n"
              ++ P.render (P.nest 4 (P.pretty False leftType))
              ++ "\n\nand this type:\n\n"
              ++ P.render (P.nest 4 (P.pretty False rightType))

    InfiniteType var tipe ->
        Report.simple "This expression is leading me to infer an infinite type." $
          "Maybe you are trying to do some tricky recursion? Try breaking the expression\n"
          ++ "into smaller pieces. Give each piece a name and try to write down its type.\n\n"
          ++ "Type inference got stuck when type '" ++ var ++ "' needed equal to:\n\n"
          ++ P.render (P.nest 4 (P.pretty False tipe))
          ++ "\n\nNotice that type variable '" ++ var ++ "' appears there too, so if we\n"
          ++ "expanded this type, it would just keep getting bigger and bigger."

    BadMain tipe ->
        Report.simple "The 'main' value has an unsupported type." $
          "I need an Element, Html, (Signal Element), or (Signal Html) so I can render it\n"
          ++ "on screen, but you gave me:\n\n"
          ++ P.render (P.nest 4 (P.pretty False tipe))


hintToString :: Hint -> (Maybe Region.Region, String)
hintToString hint =
  case hint of
    None ->
        ( Nothing
        , "This expression is triggering a type mismatch."
        )

    Custom message ->
        ( Nothing
        , message
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
