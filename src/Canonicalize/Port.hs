module Canonicalize.Port (check) where

import Control.Applicative ((<$>))
import qualified Data.Foldable as Foldable

import qualified AST.Declaration as D
import qualified AST.Expression.General as E
import qualified AST.Expression.Canonical as Canonical
import qualified AST.Type as T
import qualified AST.Variable as Var
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Canonicalize as Error
import qualified Reporting.Region as R
import qualified Canonicalize.Result as Result


-- CHECK FOR PORT

check
    :: R.Region
    -> String
    -> Maybe Canonical.Expr
    -> T.Canonical
    -> Result.ResultErr D.CanonicalPort
check region name maybeExpr rootType =
  D.CanonicalPort <$> checkHelp region name maybeExpr rootType rootType


checkHelp
    :: R.Region
    -> String
    -> Maybe Canonical.Expr
    -> T.Canonical
    -> T.Canonical
    -> Result.ResultErr (E.PortImpl Canonical.Expr T.Canonical)
checkHelp region name maybeExpr rootType tipe =
  case (maybeExpr, tipe) of
    (_, T.Aliased _ args t) ->
        checkHelp region name maybeExpr rootType (T.dealias args t)

    (Just expr, T.App (T.Type task) [ _x, _a ])
        | Var.isTask task ->
            Result.ok (E.Task name expr (T.Normal tipe))


    (Just expr, T.App (T.Type signal) [ arg@(T.App (T.Type task) [ _x, _a ]) ])
        | Var.isSignal signal && Var.isTask task ->
            Result.ok (E.Task name expr (T.Signal tipe arg))


    (_, T.App (T.Type signal) [arg])
        | Var.isSignal signal ->
            case maybeExpr of
              Nothing ->
                  const (E.In name (T.Signal rootType arg))
                      <$> validForeignType region name True arg arg

              Just expr ->
                  const (E.Out name expr (T.Signal rootType arg))
                      <$> validForeignType region name False arg arg

    _ ->
        case maybeExpr of
          Nothing ->
              const (E.In name (T.Normal rootType))
                  <$> validForeignType region name True rootType tipe

          Just expr ->
              const (E.Out name expr (T.Normal rootType))
                  <$> validForeignType region name False rootType tipe


-- CHECK INBOUND AND OUTBOUND TYPES

validForeignType
    :: R.Region
    -> String
    -> Bool
    -> T.Canonical
    -> T.Canonical
    -> Result.ResultErr ()
validForeignType region name isInbound rootType tipe =
    let valid localType =
            validForeignType region name isInbound rootType localType

        err problem =
            Result.err $ A.A region $
                Error.port name isInbound rootType tipe problem
    in
    case tipe of
      T.Aliased _ args t ->
          valid (T.dealias args t)

      T.Type v ->
          case any ($ v) [ Var.isJson, Var.isPrimitive, Var.isTuple ] of
            True -> Result.ok ()
            False -> err Nothing

      T.App t [] ->
          valid t

      T.App (T.Type v) [t]
          | Var.isMaybe v -> valid t
          | Var.isArray v -> valid t
          | Var.isList  v -> valid t

      T.App (T.Type v) ts
          | Var.isTuple v ->
              Foldable.traverse_ valid ts

      T.App _ _ ->
          err Nothing

      T.Var _ ->
          err (Just "a free type variable")

      T.Lambda _ _ ->
          err (Just "a function")

      T.Record _ (Just _) ->
          err (Just "an extended record")

      T.Record fields Nothing ->
          Foldable.traverse_ (\(k,v) -> (,) k <$> valid v) fields
