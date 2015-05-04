module Transform.Canonicalize.Port (check) where

import Control.Applicative ((<$>))
import qualified Data.Foldable as Foldable

import qualified AST.Declaration as D
import qualified AST.Expression.General as E
import qualified AST.Expression.Canonical as Canonical
import qualified AST.Type as T
import qualified AST.Variable as Var
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Canonicalize as Error
import qualified Transform.Canonicalize.Result as Result


-- CHECK FOR PORT

check
    :: String
    -> Maybe Canonical.Expr
    -> T.Canonical
    -> Result.ResultErr D.CanonicalPort
check name maybeExpr rootType =
  D.CanonicalPort <$> checkHelp name maybeExpr rootType rootType


checkHelp
    :: String
    -> Maybe Canonical.Expr
    -> T.Canonical
    -> T.Canonical
    -> Result.ResultErr (E.PortImpl Canonical.Expr T.Canonical)
checkHelp name maybeExpr rootType tipe =
  case (maybeExpr, tipe) of
    (_, T.Aliased _ args t) ->
        checkHelp name maybeExpr rootType (T.dealias args t)

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
                      <$> validForeignType name True arg arg

              Just expr ->
                  const (E.Out name expr (T.Signal rootType arg))
                      <$> validForeignType name False arg arg

    _ ->
        case maybeExpr of
          Nothing ->
              const (E.In name (T.Normal rootType))
                  <$> validForeignType name True rootType tipe

          Just expr ->
              const (E.Out name expr (T.Normal rootType))
                  <$> validForeignType name False rootType tipe


-- CHECK INBOUND AND OUTBOUND TYPES

validForeignType
    :: String
    -> Bool
    -> T.Canonical
    -> T.Canonical
    -> Result.ResultErr ()
validForeignType name isInbound rootType tipe =
    let valid localType =
            validForeignType name isInbound rootType localType

        err problem =
            Result.err $ A.A undefined $
                Error.port name isInbound rootType tipe problem
    in
    case tipe of
      T.Aliased _ args t ->
          valid (T.dealias args t)

      T.Type v ->
          case any ($ v) [ Var.isJson, Var.isPrimitive, Var.isTuple ] of
            True -> Result.ok ()
            False -> err "an unsupported type"

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
          err "an unsupported type"

      T.Var _ ->
          err "a free type variable"

      T.Lambda _ _ ->
          err "functions"

      T.Record _ (Just _) ->
          err "extended records with free type variables"

      T.Record fields Nothing ->
          Foldable.traverse_ (\(k,v) -> (,) k <$> valid v) fields
