{-# OPTIONS_GHC -Wall #-}
module Reporting.Error.Type where

import qualified Text.PrettyPrint as P

import qualified AST.Type as T
import qualified Reporting.PrettyPrint as P
import qualified Reporting.Report as Report


data Error
    = Mismatch Mismatch
    | InfiniteType String T.Canonical
    | BadMain T.Canonical


data Mismatch = MismatchInfo
    { _hint :: Maybe String
    , _leftType :: T.Canonical
    , _rightType :: T.Canonical
    }


-- TO REPORT

toReport :: Error -> Report.Report
toReport err =
  case err of
    Mismatch (MismatchInfo maybeHint leftType rightType) ->
        let
          preHint =
            maybe "This expression is triggering a type mismatch." id maybeHint

          postHint =
            "During type inference, I am seeing a conflict between this type:\n\n"
            ++ P.render (P.nest 4 (P.pretty False leftType))
            ++ "\n\nand this type:\n\n"
            ++ P.render (P.nest 4 (P.pretty False rightType))
        in
          Report.simple preHint postHint

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
