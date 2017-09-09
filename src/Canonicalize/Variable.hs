{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Canonicalize.Variable (Result, variable, tvar, pvar) where

import Data.Monoid ((<>))
import qualified Data.Either as Either
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Text (Text)

import qualified AST.Helpers as Help
import qualified AST.Module.Name as ModuleName
import qualified AST.Type as Type
import qualified AST.Variable as Var
import qualified Canonicalize.Environment as Env
import qualified Reporting.Error.Canonicalize as Error
import qualified Reporting.Helpers as Help (nearbyNames)
import qualified Reporting.Region as R
import qualified Reporting.Result as Result
import qualified Reporting.Warning as Warning
import Elm.Utils ((|>))



-- RESULT HELPERS


type Result a =
  Result.Result (Set.Set ModuleName.Raw) Warning.Warning Error.Error a


logVarWith :: (a -> Var.Canonical) -> a -> Result a
logVarWith toVar value =
  case toVar value of
    Var.Canonical (Var.Module (ModuleName.Canonical _ moduleName)) _name ->
      Result.accumulate (Set.singleton moduleName) value

    Var.Canonical _ _ ->
      Result.ok value


logVar :: Var.Canonical -> Result Var.Canonical
logVar var =
  logVarWith id var



-- FINDERS


variable :: R.Region -> Env.Env -> Text -> Result Var.Canonical
variable region env var =
  case toVarName var of
    Right (name, varName) | ModuleName.isKernel name ->
      let
        localPkg =
          ModuleName._package (Env._home env)

        moduleName =
          ModuleName.Canonical localPkg name
      in
        logVar (Var.Canonical (Var.Module moduleName) varName)

    _ ->
      case Set.toList <$> Map.lookup var (Env._values env) of
        Just [v] ->
          logVar v

        Just vs ->
          preferLocals region env Error.BadValue vs var

        Nothing ->
          notFound region Error.BadValue (Map.keys (Env._values env)) var


tvar :: R.Region -> Env.Env -> Text -> Result (Either Var.Canonical (Var.Canonical, [Text], Type.Canonical))
tvar region env var =
  case unions ++ aliases of
    []  -> notFound region Error.BadType (Map.keys (Env._unions env) ++ Map.keys (Env._aliases env)) var
    [v] -> logVarWith extract v
    vs  -> preferLocals' region env extract Error.BadType vs var
  where
    unions =
        map Left (maybe [] Set.toList (Map.lookup var (Env._unions env)))

    aliases =
        map Right (maybe [] Set.toList (Map.lookup var (Env._aliases env)))

    extract value =
        case value of
          Left v -> v
          Right (v,_,_) -> v


pvar :: R.Region -> Env.Env -> Text -> Int -> Result Var.Canonical
pvar region env var actualArgs =
  case Set.toList <$> Map.lookup var (Env._patterns env) of
    Just [value] ->
        foundArgCheck value

    Just values ->
        foundArgCheck =<< preferLocals' region env fst Error.BadPattern values var

    Nothing ->
        notFound region Error.BadPattern (Map.keys (Env._patterns env)) var
  where
    foundArgCheck (name, expectedArgs) =
        if actualArgs == expectedArgs then
          logVar name

        else
          Result.throw region (Error.argMismatch name expectedArgs actualArgs)



-- FOUND


preferLocals :: R.Region -> Env.Env -> Error.VarKind -> [Var.Canonical] -> Text -> Result Var.Canonical
preferLocals region env =
  preferLocals' region env id


preferLocals'
    :: R.Region
    -> Env.Env
    -> (a -> Var.Canonical)
    -> Error.VarKind
    -> [a]
    -> Text
    -> Result a
preferLocals' region env extract kind possibilities var =
    case filter (isLocal (Env._home env) . extract) possibilities of
      [] ->
          ambiguous possibilities

      [v] ->
          logVarWith extract v

      --Local vars shadow top-level vars
      [v1, v2]
          | isTopLevel (extract v1) ->
              logVarWith extract v2

          | isTopLevel (extract v2) ->
              logVarWith extract v1

      locals ->
          ambiguous locals
    where
      ambiguous possibleVars =
          Result.throw region (Error.variable kind var Error.Ambiguous vars)
        where
          vars = map (Var.toText . extract) possibleVars


isLocal :: ModuleName.Canonical -> Var.Canonical -> Bool
isLocal contextName (Var.Canonical home _) =
  case home of
    Var.Local ->
        True

    Var.TopLevel _ ->
        True

    Var.BuiltIn ->
        False

    Var.Module name ->
        name == contextName


isTopLevel :: Var.Canonical -> Bool
isTopLevel (Var.Canonical home _) =
  case home of
    Var.TopLevel _ ->
        True

    _ ->
        False



-- NOT FOUND HELPERS


type VarName =
    Either Text (ModuleName.Raw, Text)


toVarName :: Text -> VarName
toVarName var =
  case Text.splitOn "." var of
    [x] -> Left x
    xs -> Right (Text.intercalate "." (init xs), last xs)


noQualifier :: VarName -> Text
noQualifier name =
  case name of
    Right (_, x) -> x
    Left x       -> x


qualifiedToText :: (ModuleName.Raw, Text) -> Text
qualifiedToText (modul, name) =
  ModuleName.toText modul <> "." <> name


isOp :: VarName -> Bool
isOp name =
  Help.isOp (noQualifier name)



-- NOT FOUND


notFound :: R.Region -> Error.VarKind -> [Text] -> Text -> Result a
notFound region kind possibilities var =
  let name =
          toVarName var

      possibleNames =
          map toVarName possibilities

      (problem, suggestions) =
          case name of
            Left _ ->
                exposedProblem name possibleNames

            Right (modul, varName) ->
                qualifiedProblem modul varName (Either.rights possibleNames)
    in
        Result.throw region (Error.variable kind var problem suggestions)


exposedProblem :: VarName -> [VarName] -> (Error.VarProblem, [Text])
exposedProblem name possibleNames =
  let (exposed, qualified) =
          possibleNames
            |> filter (\n -> isOp name == isOp n)
            |> Help.nearbyNames noQualifier name
            |> Either.partitionEithers
  in
      ( Error.ExposedUnknown
      , exposed ++ map qualifiedToText qualified
      )


qualifiedProblem :: ModuleName.Raw -> Text -> [(ModuleName.Raw, Text)] -> (Error.VarProblem, [Text])
qualifiedProblem moduleName name allQualified =
  let availableModules =
        Set.fromList (map fst allQualified)
  in
      case Set.member moduleName availableModules of
        True ->
            ( Error.QualifiedUnknown moduleName name
            , allQualified
                |> filter ((==) moduleName . fst)
                |> map snd
                |> Help.nearbyNames id name
            )

        False ->
            ( Error.UnknownQualifier moduleName name
            , Set.toList availableModules
                |> Help.nearbyNames id moduleName
            )
