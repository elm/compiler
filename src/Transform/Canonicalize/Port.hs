module Transform.Canonicalize.Port (check) where

import Control.Applicative ((<$>))
import Control.Monad.Error (throwError)
import Text.PrettyPrint as P

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
    -> T.CanonicalType
    -> Env.Canonicalizer [P.Doc] (T.PortType Var.Canonical)
check name tipe =
  checkHelp name tipe tipe


checkHelp
    :: String
    -> T.CanonicalType
    -> T.CanonicalType
    -> Env.Canonicalizer [P.Doc] (T.PortType Var.Canonical)
checkHelp name rootType tipe =
  case tipe of
    T.Aliased alias _ arg
        | Var.is ["Port"] "Port" alias ->
            return (T.Internal arg)

        | Var.is ["Port"] "InboundPort" alias ->
            do  validForeignType name In arg arg
                return (T.Inbound arg)

        | Var.is ["Port"] "OutboundPort" alias ->
            do  validForeignType name Out arg arg
                return (T.Outbound arg)

        | otherwise ->
            checkHelp name rootType arg

    _ ->
        throw $
          [ P.text "Port Error:"
          , P.nest 4 $
              P.vcat $
                [ P.text ("The port named '" ++ name ++ "' has an invalid type.\n")
                , P.nest 4 (PP.pretty tipe) <> P.text "\n"
                , P.text "It must be a Port, InboundPort, or OutboundPort."
                , P.text "See the Port module for more details:"
                , P.text "<http://package.elm-lang.org/packages/elm-lang/core/latest/Port>"
                ]
          ]


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
          , txt [ "Acceptable values for ", port, "s include:" ]
          , txt [ "    Ints, Floats, Bools, Strings, Maybes, Lists, Arrays," ]
          , txt [ "    Tuples, unit values, Json.Values, and concrete records." ]
          ]
    ]
  where
    port =
        case portKind of
          In -> "inbound port"
          Out -> "outbound port"

    txt = P.text . concat