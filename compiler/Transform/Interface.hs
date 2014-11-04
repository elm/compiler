module Transform.Interface (filterExports) where

import qualified Data.Map as Map
import qualified AST.Module as Module
import qualified AST.Type as Type
import qualified AST.Variable as Var

filterExports :: Module.Interface -> Module.Interface
filterExports interface =
    interface
    { Module.iTypes =
        Map.fromList (concatMap getTypes exportedValues)
    , Module.iAliases =
        Map.fromList (concatMap getAliases exportedValues)
    , Module.iAdts =
        Map.fromList (concatMap getAdts exportedValues)
    }
  where
    exportedValues :: [Var.Value]
    exportedValues = Module.iExports interface

    get :: Map.Map String a -> String -> [(String, a)]
    get dict x =
        case Map.lookup x dict of
          Just t  -> [(x,t)]
          Nothing -> []

    getTypes :: Var.Value -> [(String, Type.CanonicalType)]
    getTypes value =
        case value of
          Var.Value x -> getType x
          Var.Alias _ -> []
          Var.ADT _ (Var.Listing ctors _) -> concatMap getType ctors

    getType :: String -> [(String, Type.CanonicalType)]
    getType name =
        get (Module.iTypes interface) name

    getAliases :: Var.Value -> [(String, ([String], Type.CanonicalType))]
    getAliases value =
        case value of
          Var.Value _ -> []
          Var.Alias name -> get (Module.iAliases interface) name
          Var.ADT _ _ -> []

    getAdts :: Var.Value -> [(String, Module.AdtInfo String)]
    getAdts value =
        case value of
          Var.Value _ -> []
          Var.Alias _ -> []
          Var.ADT name (Var.Listing exportedCtors _) ->
              case Map.lookup name (Module.iAdts interface) of
                Nothing -> []
                Just (tvars, ctors) ->
                    [(name, (tvars, filter isExported ctors))]
                  where
                    isExported (ctor, _) = ctor `elem` exportedCtors