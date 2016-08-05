{-# OPTIONS_GHC -Wall #-}
module Canonicalize.Variable (Result, variable, tvar, pvar) where

import qualified Data.Either as Either
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified AST.Helpers as Help
import qualified AST.Module.Name as ModuleName
import qualified AST.Type as Type
import qualified AST.Variable as Var
import qualified Canonicalize.Environment as Env
import qualified Reporting.Error.Canonicalize as Error
import qualified Reporting.Error.Helpers as Error
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


variable :: R.Region -> Env.Environment -> String -> Result Var.Canonical
variable region env var =
  case toVarName var of
    Right (name, varName) | ModuleName.isNative name ->
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
          preferLocals region env "variable" vs var

        Nothing ->
          notFound region "variable" (Map.keys (Env._values env)) var


tvar
    :: R.Region
    -> Env.Environment
    -> String
    -> Result
          (Either
              Var.Canonical
              (Var.Canonical, [String], Type.Canonical)
          )
tvar region env var =
  case unions ++ aliases of
    []  -> notFound region "type" (Map.keys (Env._unions env) ++ Map.keys (Env._aliases env)) var
    [v] -> logVarWith extract v
    vs  -> preferLocals' region env extract "type" vs var
  where
    unions =
        map Left (maybe [] Set.toList (Map.lookup var (Env._unions env)))

    aliases =
        map Right (maybe [] Set.toList (Map.lookup var (Env._aliases env)))

    extract value =
        case value of
          Left v -> v
          Right (v,_,_) -> v


pvar
    :: R.Region
    -> Env.Environment
    -> String
    -> Int
    -> Result Var.Canonical
pvar region env var actualArgs =
  case Set.toList <$> Map.lookup var (Env._patterns env) of
    Just [value] ->
        foundArgCheck value

    Just values ->
        foundArgCheck =<< preferLocals' region env fst "pattern" values var

    Nothing ->
        notFound region "pattern" (Map.keys (Env._patterns env)) var
  where
    foundArgCheck (name, expectedArgs) =
        if actualArgs == expectedArgs then
          logVar name

        else
          Result.throw region (Error.argMismatch name expectedArgs actualArgs)



-- FOUND


preferLocals
    :: R.Region
    -> Env.Environment
    -> String
    -> [Var.Canonical]
    -> String
    -> Result Var.Canonical
preferLocals region env =
  preferLocals' region env id


preferLocals'
    :: R.Region
    -> Env.Environment
    -> (a -> Var.Canonical)
    -> String
    -> [a]
    -> String
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
          vars = map (Var.toString . extract) possibleVars


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
    Either String (ModuleName.Raw, String)


toVarName :: String -> VarName
toVarName var =
  case Help.splitDots var of
    [x] -> Left x
    xs -> Right (init xs, last xs)


noQualifier :: VarName -> String
noQualifier name =
  case name of
    Left x -> x
    Right (_, x) -> x


qualifiedToString :: (ModuleName.Raw, String) -> String
qualifiedToString (modul, name) =
  ModuleName.toString modul ++ "." ++ name


isOp :: VarName -> Bool
isOp name =
  Help.isOp (noQualifier name)



-- NOT FOUND


notFound :: R.Region -> String -> [String] -> String -> Result a
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


exposedProblem :: VarName -> [VarName] -> (Error.VarProblem, [String])
exposedProblem name possibleNames =
  let (exposed, qualified) =
          possibleNames
            |> filter (\n -> isOp name == isOp n)
            |> Error.nearbyNames noQualifier name
            |> Either.partitionEithers
  in
      ( Error.ExposedUnknown
      , exposed ++ map qualifiedToString qualified
      )


qualifiedProblem
    :: ModuleName.Raw
    -> String
    -> [(ModuleName.Raw, String)]
    -> (Error.VarProblem, [String])
qualifiedProblem moduleName name allQualified =
  let availableModules =
        Set.fromList (map fst allQualified)

      moduleNameString =
        ModuleName.toString moduleName
  in
      case Set.member moduleName availableModules of
        True ->
            ( Error.QualifiedUnknown moduleNameString name
            , allQualified
                |> filter ((==) moduleName . fst)
                |> map snd
                |> Error.nearbyNames id name
            )

        False ->
            ( Error.UnknownQualifier moduleNameString name
            , Set.toList availableModules
                |> map ModuleName.toString
                |> Error.nearbyNames id moduleNameString
            )
