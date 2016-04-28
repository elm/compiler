module Canonicalize.Effects (canonicalize, toValues, checkPortType) where

import qualified Data.Foldable as F
import qualified Data.Traversable as T

import qualified AST.Effects as Effects
import qualified AST.Type as T
import qualified AST.Variable as Var
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Canonicalize as Error
import qualified Reporting.Region as R
import qualified Reporting.Result as Result
import qualified Canonicalize.Environment as Env
import qualified Canonicalize.Type as Canonicalize
import Canonicalize.Variable (Result)



-- TO EXPORT VALUES


toValues :: Effects.Raw -> [Var.Value]
toValues effects =
  case effects of
    Effects.None ->
      []

    Effects.Manager _ _ ->
      []

    Effects.Port ports ->
      map (Var.Value . Effects._rawName . A.drop) ports



-- CANONICALIZE


canonicalize :: Env.Environment -> Effects.Raw -> Result Effects.Canonical
canonicalize env effects =
  case effects of
    Effects.None ->
      Result.ok Effects.None

    Effects.Manager _ info ->
      Result.ok (Effects.Manager (Env.getPackage env) info)

    Effects.Port rawPorts ->
      Effects.Port <$> T.traverse (canonicalizeRawPort env) rawPorts


canonicalizeRawPort
  :: Env.Environment
  -> A.Commented Effects.PortRaw
  -> Result (A.Commented Effects.PortCanonical)
canonicalizeRawPort env (A.A ann (Effects.PortRaw name rawType)) =
  do  tipe <- Canonicalize.tipe env rawType
      kind <- figureOutKind (fst ann) name tipe
      Result.ok (A.A ann (Effects.PortCanonical name kind tipe))


figureOutKind :: R.Region -> String -> T.Canonical -> Result Effects.Kind
figureOutKind region name rootType =
  case T.deepDealias rootType of
    T.Lambda outgoingType (T.App (T.Type effect) [T.Var _])
      | effect == Var.cmd ->
          pure (Effects.Outgoing outgoingType)
            <* checkPortType (makeError region name) outgoingType

    T.Lambda (T.Lambda incomingType (T.Var msg1)) (T.App (T.Type effect) [T.Var msg2])
      | effect == Var.sub && msg1 == msg2 ->
          pure (Effects.Incoming incomingType)
            <* checkPortType (makeError region name) incomingType

    _ ->
      Result.throw region (Error.BadPort name rootType)


makeError :: R.Region -> String -> T.Canonical -> Maybe String -> A.Located Error.Error
makeError region name tipe maybeMessage =
  A.A region (Error.port name tipe maybeMessage)



-- CHECK INCOMING AND OUTGOING TYPES


checkPortType
  :: (Monoid i)
  => (T.Canonical -> Maybe String -> A.Located e)
  -> T.Canonical
  -> Result.Result i w e ()
checkPortType makeError tipe =
  let
    check =
      checkPortType makeError

    throw maybeMsg =
      Result.throwMany [makeError tipe maybeMsg]
  in
    case tipe of
      T.Aliased _ args aliasedType ->
        check (T.dealias args aliasedType)

      T.Type name ->
        if Var.isJson name || Var.isPrimitive name || Var.isTuple name then
          return ()

        else
          throw Nothing

      T.App name [] ->
          check name

      T.App (T.Type name) [arg]
          | Var.isMaybe name -> check arg
          | Var.isArray name -> check arg
          | Var.isList  name -> check arg

      T.App (T.Type name) args
          | Var.isTuple name ->
              F.traverse_ check args

      T.App _ _ ->
          throw Nothing

      T.Var _ ->
          throw (Just "free type variable")

      T.Lambda _ _ ->
          throw (Just "function")

      T.Record _ (Just _) ->
          throw (Just "extended record")

      T.Record fields Nothing ->
          F.traverse_ (\(k,v) -> (,) k <$> check v) fields
