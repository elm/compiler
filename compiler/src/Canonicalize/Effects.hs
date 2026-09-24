{-# LANGUAGE QuasiQuotes #-}
module Canonicalize.Effects
  ( canonicalize
  , checkPayload
  )
  where

import qualified Data.Foldable as F
import qualified Data.Map as Map

import qualified AST.Canonical as Can
import qualified AST.Source as Src
import qualified AST.Prim.Name as N
import qualified AST.Prim.TypeName as T
import qualified AST.Utils.Type as Type
import qualified Canonicalize.Environment as Env
import qualified Canonicalize.Type as Type
import qualified Elm.ModuleName as ModuleName
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Canonicalize as Error
import qualified Reporting.Result as Result



-- RESULT


type Result i w a =
  Result.Result i w Error.Error a



-- CANONICALIZE


canonicalize
  :: Env.Env
  -> [A.Located Src.Value]
  -> Map.Map T.Name union
  -> Src.Effects
  -> Result i w Can.Effects
canonicalize env values unions effects =
  case effects of
    Src.NoEffects ->
      Result.ok Can.NoEffects

    Src.Ports ports ->
      do  pairs <- traverse (canonicalizePort env) ports
          return $ Can.Ports (Map.fromList pairs)

    Src.Manager region manager ->
      let dict = Map.fromList (map toNameRegion values) in
      Can.Manager
        <$> verifyManager region dict [N.ascii|init|]
        <*> verifyManager region dict [N.ascii|onEffects|]
        <*> verifyManager region dict [N.ascii|onSelfMsg|]
        <*>
          case manager of
            Src.Cmd cmdType ->
              Can.Cmd
                <$> verifyEffectType cmdType unions
                <*  verifyManager region dict [N.ascii|cmdMap|]

            Src.Sub subType ->
              Can.Sub
                <$> verifyEffectType subType unions
                <*  verifyManager region dict [N.ascii|subMap|]

            Src.Fx cmdType subType ->
              Can.Fx
                <$> verifyEffectType cmdType unions
                <*> verifyEffectType subType unions
                <*  verifyManager region dict [N.ascii|cmdMap|]
                <*  verifyManager region dict [N.ascii|subMap|]



-- CANONICALIZE PORT


canonicalizePort :: Env.Env -> Src.Port -> Result i w (N.Name, Can.Port)
canonicalizePort env (Src.Port (A.At region portName) tipe) =
  do  (Can.Forall freeVars ctipe) <- Type.toAnnotation env tipe
      case reverse (Type.delambda (Type.deepDealias ctipe)) of
        Can.TType home name [msg] : revArgs
           | home == ModuleName.platform_cmd && name == T.cmd ->
                case revArgs of
                  [] ->
                    Result.throw (Error.PortTypeInvalid region portName Error.CmdNoArg)

                  [outgoingType] ->
                    case msg of
                      Can.TVar _ ->
                        case checkPayload outgoingType of
                          Right () ->
                            Result.ok (portName, Can.Outgoing freeVars outgoingType ctipe)

                          Left (badType, err) ->
                            Result.throw (Error.PortPayloadInvalid region portName badType err)

                      _ ->
                        Result.throw (Error.PortTypeInvalid region portName Error.CmdBadMsg)

                  _ ->
                    Result.throw (Error.PortTypeInvalid region portName (Error.CmdExtraArgs (length revArgs)))

            | home == ModuleName.platform_sub && name == T.sub ->
                case revArgs of
                  [Can.TLambda incomingType (Can.TVar msg1)] ->
                    case msg of
                      Can.TVar msg2 | msg1 == msg2 ->
                        case checkPayload incomingType of
                          Right () ->
                            Result.ok (portName, Can.Incoming freeVars incomingType ctipe)

                          Left (badType, err) ->
                            Result.throw (Error.PortPayloadInvalid region portName badType err)

                      _ ->
                        Result.throw (Error.PortTypeInvalid region portName Error.SubBad)

                  _ ->
                    Result.throw (Error.PortTypeInvalid region portName Error.SubBad)

        _ ->
          Result.throw (Error.PortTypeInvalid region portName Error.NotCmdOrSub)



-- VERIFY MANAGER


verifyEffectType :: A.Located T.Name -> Map.Map T.Name a -> Result i w T.Name
verifyEffectType (A.At region name) unions =
  if Map.member name unions then
    Result.ok name
  else
    Result.throw (Error.EffectNotFound region name)


toNameRegion :: A.Located Src.Value -> (N.Name, A.Region)
toNameRegion (A.At _ (Src.Value (A.At region name) _ _ _)) =
  (name, region)


verifyManager :: A.Region -> Map.Map N.Name A.Region -> N.Name -> Result i w A.Region
verifyManager tagRegion values name =
  case Map.lookup name values of
    Just region ->
      Result.ok region

    Nothing ->
      Result.throw (Error.EffectFunctionNotFound tagRegion name)



-- CHECK PAYLOAD TYPES


checkPayload :: Can.Type -> Either (Can.Type, Error.InvalidPayload) ()
checkPayload tipe =
  case tipe of
    Can.TAlias _ _ args aliasedType ->
      checkPayload (Type.dealias args aliasedType)

    Can.TType home name args ->
      case args of
        []
          | isJson home name -> Right ()
          | isString home name -> Right ()
          | isIntFloatBool home name -> Right ()

        [arg]
          | isList  home name -> checkPayload arg
          | isMaybe home name -> checkPayload arg
          | isArray home name -> checkPayload arg

        _ ->
          Left (tipe, Error.UnsupportedType name)

    Can.TUnit ->
        Right ()

    Can.TTuple a b maybeC ->
        do  checkPayload a
            checkPayload b
            case maybeC of
              Nothing ->
                Right ()

              Just c ->
                checkPayload c

    Can.TVar name ->
        Left (tipe, Error.TypeVariable name)

    Can.TLambda _ _ ->
        Left (tipe, Error.Function)

    Can.TRecord _ (Just _) ->
        Left (tipe, Error.ExtendedRecord)

    Can.TRecord fields Nothing ->
        F.traverse_ checkFieldPayload fields


checkFieldPayload :: Can.FieldType -> Either (Can.Type, Error.InvalidPayload) ()
checkFieldPayload (Can.FieldType _ tipe) =
  checkPayload tipe


isIntFloatBool :: ModuleName.Canonical -> T.Name -> Bool
isIntFloatBool home name =
  home == ModuleName.basics
  &&
  (name == T.int || name == T.float || name == T.bool)


isString :: ModuleName.Canonical -> T.Name -> Bool
isString home name =
  home == ModuleName.string
  &&
  name == T.string


isJson :: ModuleName.Canonical -> T.Name -> Bool
isJson home name =
  home == ModuleName.jsonEncode
  &&
  name == T.value


isList :: ModuleName.Canonical -> T.Name -> Bool
isList home name =
  home == ModuleName.list
  &&
  name == T.list


isMaybe :: ModuleName.Canonical -> T.Name -> Bool
isMaybe home name =
  home == ModuleName.maybe
  &&
  name == T.maybe


isArray :: ModuleName.Canonical -> T.Name -> Bool
isArray home name =
  home == ModuleName.array
  &&
  name == T.array
