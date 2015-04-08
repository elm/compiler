module Transform.Canonicalize.Port (check) where

import Control.Applicative ((<$>))
import Control.Monad.Error (throwError)
import Text.PrettyPrint as P

import qualified AST.Declaration as D
import qualified AST.Expression.General as E
import qualified AST.Expression.Canonical as Canonical
import qualified AST.PrettyPrint as PP
import qualified AST.Type as T
import qualified AST.Variable as Var
import qualified Transform.Canonicalize.Environment as Env


throw :: [Doc] -> Env.Canonicalizer [P.Doc] a
throw err =
  throwError [ P.vcat err ]


-- CHECK FOR PORT

check
    :: String
    -> Maybe Canonical.Expr
    -> T.CanonicalType
    -> Env.Canonicalizer [P.Doc] D.CanonicalPort
check name maybeExpr rootType =
  do  impl <- checkHelp name maybeExpr rootType rootType
      return (D.CanonicalPort impl)


checkHelp
    :: String
    -> Maybe Canonical.Expr
    -> T.CanonicalType
    -> T.CanonicalType
    -> Env.Canonicalizer [P.Doc] (E.PortImpl Canonical.Expr Var.Canonical)
checkHelp name maybeExpr rootType tipe =
  case (maybeExpr, tipe) of
    (_, T.Aliased _ args t) ->
        checkHelp name maybeExpr rootType (T.dealias args t)

    (Just expr, T.App (T.Type task) [ _x, _a ])
        | Var.isTask task ->
            return (E.Task name expr (T.Normal tipe))


    (Just expr, T.App (T.Type signal) [ arg@(T.App (T.Type task) [ _x, _a ]) ])
        | Var.isSignal signal && Var.isTask task ->
            return (E.Task name expr (T.Signal tipe arg))


    (_, T.App (T.Type signal) [arg])
        | Var.isSignal signal ->
            case maybeExpr of
              Nothing ->
                  do  validForeignType name In arg arg
                      return (E.In name (T.Signal rootType arg))

              Just expr ->
                  do  validForeignType name Out arg arg
                      return (E.Out name expr (T.Signal rootType arg))

    _ ->
        case maybeExpr of
          Nothing ->
              do  validForeignType name In rootType tipe
                  return (E.In name (T.Normal rootType))

          Just expr ->
              do  validForeignType name Out rootType tipe
                  return (E.Out name expr (T.Normal rootType))


-- CHECK INBOUND AND OUTBOUND TYPES

data Direction = In | Out


validForeignType
    :: String
    -> Direction
    -> T.CanonicalType
    -> T.CanonicalType
    -> Env.Canonicalizer [P.Doc] ()
validForeignType name portKind rootType tipe =
    let valid localType =
            validForeignType name portKind rootType localType

        err hint =
            throw (foreignError name portKind rootType tipe hint)
    in
    case tipe of
      T.Aliased _ args t ->
          valid (T.dealias args t)

      T.Type v ->
          case any ($ v) [ Var.isJson, Var.isPrimitive, Var.isTuple ] of
            True -> return ()
            False -> err "It contains an unsupported type"

      T.App t [] ->
          valid t

      T.App (T.Type v) [t]
          | Var.isMaybe v -> valid t
          | Var.isArray v -> valid t
          | Var.isList  v -> valid t

      T.App (T.Type v) ts
          | Var.isTuple v -> mapM_ valid ts

      T.App _ _ ->
          err "It contains an unsupported type"

      T.Var _ ->
          err "It contains a free type variable"

      T.Lambda _ _ ->
          err "It contains functions"

      T.Record _ (Just _) ->
          err "It contains extended records with free type variables"

      T.Record fields Nothing ->
          mapM_ (\(k,v) -> (,) k <$> valid v) fields


foreignError
    :: String
    -> Direction
    -> T.CanonicalType
    -> T.CanonicalType
    -> String
    -> [P.Doc]
foreignError name portKind rootType localType problemMessage =
    [ P.text ("Port Error:")
    , P.nest 4 $
        P.vcat
          [ txt [ "The ", port, " named '", name, "' has an invalid type.\n" ]
          , P.nest 4 (PP.pretty rootType) <> P.text "\n"
          , txt [ problemMessage, ":\n" ]
          , P.nest 4 (PP.pretty localType) <> P.text "\n"
          , txt [ "The kinds of values that can flow through ", port, "s include:" ]
          , txt [ "    Ints, Floats, Bools, Strings, Maybes, Lists, Arrays," ]
          , txt [ "    Tuples, JavaScript.Values, and concrete records." ]
          ]
    ]
  where
    port =
        case portKind of
          In -> "inbound port"
          Out -> "outbound port"

    txt = P.text . concat