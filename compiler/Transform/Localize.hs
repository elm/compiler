module Transform.Localize where

import Data.List (foldl')
import Data.Map (Map)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Set (Set)
import qualified AST.Module as Module
import qualified AST.Variable as Var
import qualified AST.Type as Type
import qualified Data.Map as Map
import qualified Data.Set as Set

data Import = Import
    { _unqualified :: Set String
    , _qualifier :: Maybe String
    } deriving (Show)

defaultImport :: Import
defaultImport = Import Set.empty Nothing

extractType :: Var.Value -> Maybe String
extractType value =
  case value of
    Var.Alias alias -> Just alias
    Var.ADT name _ -> Just name
    Var.Value _ -> Nothing

type Environment = Map String Import

addQualifier :: String -> String -> Environment -> Environment
addQualifier name qualifier env =
  let oldRecord = fromMaybe defaultImport (Map.lookup name env)
  in Map.insert name (oldRecord { _qualifier = Just qualifier }) env

addUnqualified :: String -> Set String -> Environment -> Environment
addUnqualified name unqualified env =
  let oldRecord = fromMaybe defaultImport (Map.lookup name env)
  in Map.insert name (oldRecord { _unqualified = unqualified }) env

environment :: Module.CanonicalModule -> Environment
environment modul = foldl' insert Map.empty ls
  where
    this = (Module.nameToString $ Module.names modul, Module.open)
    ls = this : Module.imports modul

    insert env (name, im) =
      case im of
        Module.As qname ->
          addQualifier name qname env
        Module.Open (Var.Listing expl open) ->
          case open of
            False -> Map.insert name defaultImport env
            True ->
              let types = Set.fromList $ catMaybes $ map extractType expl
              in addUnqualified name types env

localize :: String -> Import -> String
localize name (Import unqualified qualifier) =
  case qualifier of
    Nothing -> name
    Just qualifier' ->
      case Set.member name unqualified of
        True -> name
        False -> qualifier' ++ "." ++ name

var :: Map String Import -> Var.Canonical -> String
var env (Var.Canonical home name) =
  case home of
    Var.Module mod ->
      case Map.lookup mod env of
        Nothing -> mod ++ "." ++ name
        Just imp -> localize name imp
    Var.Local -> name
    Var.BuiltIn -> name

tipe :: Map String Import -> Type.CanonicalType -> Type.RawType
tipe env = go
  where
    go :: Type.CanonicalType -> Type.RawType
    go typ =
      case typ of
        Type.Lambda t1 t2 ->
          Type.Lambda (go t1) (go t2)

        Type.Var x ->
          Type.Var x

        Type.Type v ->
          Type.Type $ Var.Raw $ var env v

        Type.App fn args ->
          Type.App (go fn) $ map go args

        Type.Record fields ext ->
          let f (x, y) = (x, go y)
          in Type.Record (map f fields) $ fmap go ext

        Type.Aliased v _ ->
          Type.Type $ Var.Raw $ var env v
