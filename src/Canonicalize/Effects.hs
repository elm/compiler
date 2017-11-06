{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Canonicalize.Effects
  ( canonicalize
  , checkPayload
  )
  where

import qualified Data.Foldable as F
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text)

import qualified AST.Expression.Canonical as Can
import qualified AST.Expression.Valid as Valid
import qualified AST.Module.Name as ModuleName
import qualified AST.Type as Type
import qualified Canonicalize.Environment as Env
import qualified Canonicalize.Type as Type
import qualified Elm.Name as N
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Canonicalize as Error
import qualified Reporting.Region as R
import qualified Reporting.Result as Result
import qualified Reporting.Warning as Warning



-- RESULT


type Result a =
  Result.Result () Warning.Warning Error.Error a



-- CANONICALIZE


canonicalize
  :: Env.Env
  -> [A.Located Valid.Decl]
  -> Map.Map N.Name union
  -> Valid.Effects
  -> Result Can.Effects
canonicalize env decls unions effects =
  case effects of
    Valid.NoEffects ->
      Result.ok Can.NoEffects

    Valid.Ports ports ->
      do  pairs <- traverse (canonicalizePort env) ports
          return $ Can.Ports (Map.fromList pairs)

    Valid.Manager region manager ->
      let dict = Map.fromList (map toNameRegion decls) in
      Can.Manager
        <$> verifyManager region dict "init"
        <*> verifyManager region dict "onEffects"
        <*> verifyManager region dict "onSelfMsg"
        <*>
          case manager of
            Valid.Cmd cmdType ->
              Can.Cmd <$> verifyEffectType cmdType unions

            Valid.Sub subType ->
              Can.Sub <$> verifyEffectType subType unions

            Valid.Fx cmdType subType ->
              Can.Fx
                <$> verifyEffectType cmdType unions
                <*> verifyEffectType subType unions



-- CANONICALIZE PORT


canonicalizePort :: Env.Env -> Valid.Port -> Result (N.Name, Can.Port)
canonicalizePort env (Valid.Port (A.A region portName) tipe) =
  do  ctipe <- Type.canonicalize env tipe
      case Type.deepDealias ctipe of
        Type.Lambda outgoingType (Type.Type home name [Type.Var _])
          | home == ModuleName.cmd && name == "Cmd" ->
              case checkPayload outgoingType of
                Left (badType, err) ->
                  Result.throw region (Error.PortPayloadInvalid portName badType err)

                Right () ->
                  Result.ok (portName, Can.Outgoing ctipe)

        Type.Lambda (Type.Lambda incomingType (Type.Var msg1)) (Type.Type home name [Type.Var msg2])
          | home == ModuleName.sub && name == "Sub" && msg1 == msg2 ->
              case checkPayload incomingType of
                Left (badType, err) ->
                  Result.throw region (Error.PortPayloadInvalid portName badType err)

                Right () ->
                  Result.ok (portName, Can.Incoming ctipe)

        _ ->
          Result.throw region (Error.PortTypeInvalid portName ctipe)



-- VERIFY MANAGER


verifyEffectType :: A.Located N.Name -> Map.Map N.Name a -> Result N.Name
verifyEffectType (A.A region name) unions =
  if Map.member name unions then
    Result.ok name
  else
    Result.throw region (Error.EffectNotFound name)


toNameRegion :: A.Located Valid.Decl -> (N.Name, R.Region)
toNameRegion (A.A _ (Valid.Decl (A.A region name) _ _ _)) =
  (name, region)


verifyManager :: R.Region -> Map.Map N.Name R.Region -> N.Name -> Result R.Region
verifyManager tagRegion decls name =
  case Map.lookup name decls of
    Just region ->
      Result.ok region

    Nothing ->
      Result.throw tagRegion (Error.EffectFunctionNotFound name)



-- CHECK PAYLOAD TYPES


checkPayload :: Type.Canonical -> Either (Type.Canonical, Error.InvalidPayload) ()
checkPayload tipe =
  case tipe of
    Type.Aliased _ _ args aliasedType ->
      checkPayload (Type.dealias args aliasedType)

    Type.Type home name args ->
      case args of
        []
          | isPrim home name -> Right ()
          | isJson home name -> Right ()

        [arg]
          | isList  home name -> checkPayload arg
          | isMaybe home name -> checkPayload arg
          | isArray home name -> checkPayload arg

        _ ->
          Left (tipe, Error.UnsupportedType name)

    Type.Unit ->
        Right ()

    Type.Tuple a b maybeC ->
        do  checkPayload a
            checkPayload b
            case maybeC of
              Nothing ->
                Right ()

              Just c ->
                checkPayload c

    Type.Var name ->
        Left (tipe, Error.TypeVariable name)

    Type.Lambda _ _ ->
        Left (tipe, Error.Function)

    Type.Record _ (Just _) ->
        Left (tipe, Error.ExtendedRecord)

    Type.Record fields Nothing ->
        F.traverse_ checkPayload fields


isPrim :: ModuleName.Canonical -> N.Name -> Bool
isPrim home name =
  home == ModuleName.basics && Set.member name primitives


primitives :: Set.Set Text
primitives =
  Set.fromList [ N.int, N.float, N.string, N.bool ]


isJson :: ModuleName.Canonical -> N.Name -> Bool
isJson home name =
  home == ModuleName.jsonEncode && name == "Value"


isList :: ModuleName.Canonical -> N.Name -> Bool
isList home name =
  home == ModuleName.list && name == N.list


isMaybe :: ModuleName.Canonical -> N.Name -> Bool
isMaybe home name =
  home == ModuleName.maybe && name == N.maybe


isArray :: ModuleName.Canonical -> N.Name -> Bool
isArray home name =
  home == ModuleName.array && name == N.array
