{-# OPTIONS_GHC -Wall #-}
module Transform.Canonicalize.Variable where

import qualified Data.Either as Either
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified AST.Helpers as Help
import qualified AST.Module as Module
import qualified AST.Type as Type
import qualified AST.Variable as Var
import Transform.Canonicalize.Environment as Env
import qualified Transform.Canonicalize.Error as Error
import qualified Transform.Canonicalize.Result as Result
import Elm.Utils ((|>))


variable :: Environment -> String -> Result.Result Var.Canonical
variable env var =
  case toVarName var of
    Right (name, varName)
        | Module.nameIsNative name ->
            Result.var (Var.Canonical (Var.Module name) varName)

    _ ->
        case Set.toList `fmap` Map.lookup var (_values env) of
          Just [v] ->
              Result.var v

          Just vs  ->
              preferLocals env "variable" vs var

          Nothing  ->
              notFound "variable" (Map.keys (_values env)) var


tvar
    :: Environment
    -> String
    -> Result.Result
          (Either
              Var.Canonical
              (Var.Canonical, [String], Type.CanonicalType)
          )
tvar env var =
  case adts ++ aliases of
    []  -> notFound "type" (Map.keys (_adts env) ++ Map.keys (_aliases env)) var
    [v] -> Result.var' extract v
    vs  -> preferLocals' env extract "type" vs var
  where
    adts =
        map Left (maybe [] Set.toList (Map.lookup var (_adts env)))

    aliases =
        map Right (maybe [] Set.toList (Map.lookup var (_aliases env)))

    extract value =
        case value of
          Left v -> v
          Right (v,_,_) -> v


pvar :: Environment -> String -> Result.Result Var.Canonical
pvar env var =
  case Set.toList `fmap` Map.lookup var (_patterns env) of
    Just [v] ->
        Result.var v

    Just vs ->
        preferLocals env "pattern" vs var

    Nothing ->
        notFound "pattern" (Map.keys (_patterns env)) var


-- FOUND

preferLocals
    :: Environment
    -> String
    -> [Var.Canonical]
    -> String
    -> Result.Result Var.Canonical
preferLocals env =
  preferLocals' env id


preferLocals'
    :: Environment
    -> (a -> Var.Canonical)
    -> String
    -> [a]
    -> String
    -> Result.Result a
preferLocals' env extract kind possibilities var =
    case filter (isLocal . extract) possibilities of
      [] ->
          ambiguous possibilities

      [v] ->
          Result.var' extract v

      locals ->
          ambiguous locals
    where
      isLocal :: Var.Canonical -> Bool
      isLocal (Var.Canonical home _) =
          case home of
            Var.Local -> True
            Var.BuiltIn -> False
            Var.Module name ->
                name == Env._home env

      ambiguous possibleVars =
          Result.err (Error.var kind var (Error.Ambiguous vars))
        where
          vars = map (Var.toString . extract) possibleVars


-- NOT FOUND HELPERS

type VarName =
    Either String (Module.Name, String)


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


qualifiedToString :: (Module.Name, String) -> String
qualifiedToString (modul, name) =
  Module.nameToString (modul ++ [name])


isOp :: VarName -> Bool
isOp name =
  Help.isOp (noQualifier name)


-- NOT FOUND

notFound :: String -> [String] -> String -> Result.Result a
notFound kind possibilities var =
  let name =
          toVarName var

      possibleNames =
          map toVarName possibilities

      problem =
          case name of
            Left _ ->
                exposedProblem name possibleNames

            Right (modul, varName) ->
                qualifiedProblem modul varName (Either.rights possibleNames)
    in
        Result.err (Error.var kind var problem)


exposedProblem :: VarName -> [VarName] -> Error.VarProblem
exposedProblem name possibleNames =
  let (exposed, qualified) =
          possibleNames
            |> filter (\n -> isOp name == isOp n)
            |> Error.nearbyNames noQualifier name
            |> Either.partitionEithers
  in
      Error.ExposedUnknown exposed (map qualifiedToString qualified)


qualifiedProblem
    :: Module.Name
    -> String
    -> [(Module.Name, String)]
    -> Error.VarProblem
qualifiedProblem moduleName name allQualified =
  let availableModules =
        Set.fromList (map fst allQualified)
  in
      case Set.member moduleName availableModules of
        True ->
            allQualified
              |> filter ((==) moduleName . fst)
              |> map snd
              |> Error.nearbyNames id name
              |> Error.QualifiedUnknown

        False ->
            Set.toList availableModules
              |> Error.nearbyNames Module.nameToString moduleName
              |> Error.UnknownQualifier
