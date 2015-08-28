{-# OPTIONS_GHC -Wall #-}
module Optimize.Substitute (subst) where

import Control.Arrow (second, (***))

import AST.Expression.General (Expr'(..), PortImpl(..))
import qualified AST.Expression.Optimized as Optimized
import qualified AST.Pattern as Pattern
import qualified AST.Variable as V
import Elm.Utils ((|>))
import qualified Reporting.Annotation as A


subst :: String -> Optimized.Expr' -> Optimized.Expr' -> Optimized.Expr'
subst old new expression =
    let f (A.A a e) = A.A a (subst old new e) in
    case expression of
      Range lo hi ->
          Range (f lo) (f hi)

      ExplicitList exprs ->
          ExplicitList (map f exprs)

      Binop op left right ->
          Binop op (f left) (f right)

      Lambda pattern body ->
          if Pattern.member old pattern
            then expression
            else Lambda pattern (f body)

      App func arg ->
          App (f func) (f arg)

      If branches finally ->
          If (map (f *** f) branches) (f finally)

      Let defs body ->
          if anyShadow
            then expression
            else Let (map substDef defs) (f body)
        where
          substDef (Optimized.Definition facts pattern expr) =
              Optimized.Definition facts pattern (f expr)

          anyShadow =
              map (\(Optimized.Definition _ p _) -> p) defs
                |> any (Pattern.member old)

      Var (V.Canonical home x) ->
          case home of
            V.Module _ ->
                expression

            V.BuiltIn ->
                expression

            V.Local ->
                if x == old then new else expression

            V.TopLevel _ ->
                if x == old then new else expression

      Case e cases ->
          Case (f e) (map substCase cases)
        where
          substCase (pattern, expr) =
              if Pattern.member old pattern
                then (pattern, expr)
                else (pattern, f expr)

      Data tag values ->
          Data tag (map f values)

      Access record field ->
          Access (f record) field

      Update record fields ->
          Update (f record) (map (second f) fields)

      Record fields ->
          Record (map (second f) fields)

      Literal _ ->
          expression

      GLShader _ _ _ ->
          expression

      Port impl ->
          Port $
            case impl of
              In _ _ ->
                  impl

              Out name expr tipe ->
                  Out name (f expr) tipe

              Task name expr tipe ->
                  Task name (f expr) tipe

      Crash _ ->
          expression
