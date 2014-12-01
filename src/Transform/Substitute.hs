{-# OPTIONS_GHC -Wall #-}
module Transform.Substitute (subst) where

import Control.Arrow (second, (***))
import qualified Data.Set as Set

import AST.Annotation
import AST.Expression.General (Expr'(..))
import qualified AST.Expression.Canonical as Canonical
import qualified AST.Pattern as Pattern
import qualified AST.Variable as V


subst :: String -> Canonical.Expr' -> Canonical.Expr' -> Canonical.Expr'
subst old new expression =
    let f (A a e) = A a (subst old new e) in
    case expression of
      Range e1 e2 ->
          Range (f e1) (f e2)

      ExplicitList exprs ->
          ExplicitList (map f exprs)

      Binop op e1 e2 ->
          Binop op (f e1) (f e2)

      Lambda pattern body
          | Set.member old (Pattern.boundVars pattern) -> expression
          | otherwise -> Lambda pattern (f body)

      App e1 e2 ->
          App (f e1) (f e2)

      MultiIf branches ->
          MultiIf (map (f *** f) branches)

      Let defs body
          | anyShadow -> expression
          | otherwise -> Let (map substDef defs) (f body)
        where
          substDef (Canonical.Definition p e t) =
              Canonical.Definition p (f e) t

          anyShadow =
              any (Set.member old . Pattern.boundVars)
                  [ pattern | Canonical.Definition pattern _ _ <- defs ]

      Var (V.Canonical home x) ->
          case home of
            V.Module _ -> expression
            V.BuiltIn -> expression
            V.Local -> if x == old then new else expression

      Case e cases ->
          Case (f e) (map substCase cases)
        where
          substCase (pattern, expr) =
              if Set.member old (Pattern.boundVars pattern)
                then (pattern, expr)
                else (pattern, f expr)

      Data tag values ->
          Data tag (map f values)

      Access record field ->
          Access (f record) field

      Remove record field ->
          Remove (f record) field

      Insert record field value ->
          Insert (f record) field (f value)

      Modify record fields ->
          Modify (f record) (map (second f) fields)

      Record fields ->
          Record (map (second f) fields)

      Literal _ -> expression

      GLShader _ _ _ -> expression

      PortIn name st ->
          PortIn name st

      PortOut name st signal ->
          PortOut name st (f signal)
