{-# OPTIONS_GHC -Wall #-}
module Transform.Canonicalize.Environment where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

import AST.Expression.General (saveEnvName)
import qualified AST.Module as Module
import qualified AST.Pattern as P
import qualified AST.Type as Type
import qualified AST.Variable as Var


-- ENVIRONMENT

data Environment = Env
    { _home     :: Module.Name
    , _values   :: Dict Var.Canonical
    , _adts     :: Dict Var.Canonical
    , _aliases  :: Dict (Var.Canonical, [String], Type.CanonicalType)
    , _patterns :: Dict Var.Canonical
    }


type Dict a =
    Map.Map String (Set.Set a)


fromPatches :: Module.Name -> [Patch] -> Environment
fromPatches moduleName patches =
  addPatches
      patches
      (Env moduleName Map.empty Map.empty Map.empty Map.empty)


addPattern :: P.Pattern var -> Environment -> Environment
addPattern pattern env =
  let patches =
        map (\x -> Value x (Var.local x)) (P.boundVarList pattern)
  in
      addPatches patches env


-- PATCHES

data Patch
    = Value String Var.Canonical
    | Union String Var.Canonical
    | Alias String (Var.Canonical, [String], Type.CanonicalType)
    | Pattern String Var.Canonical


-- ADD PATCH TO ENVIRONMENT

addPatches :: [Patch] -> Environment -> Environment
addPatches patches env =
  List.foldl' (flip addPatch) env patches


addPatch :: Patch -> Environment -> Environment
addPatch patch env =
  case patch of
    Value name var ->
        env { _values = insert name var (_values env) }

    Union name var ->
        env { _adts = insert name var (_adts env) }

    Alias name var ->
        env { _aliases = insert name var (_aliases env) }

    Pattern name var ->
        env { _patterns = insert name var (_patterns env) }


insert :: (Ord a) => String -> a -> Dict a -> Dict a
insert key value =
  Map.insertWith Set.union key (Set.singleton value)


-- PATCH HELPERS

builtinPatches :: [Patch]
builtinPatches =
  concat
    [ builtIn Value (tuples ++ [saveEnvName])
    , builtIn Union (tuples ++ ["List","Int","Float","Char","Bool","String"])
    , builtIn Pattern (tuples ++ ["::","[]"])
    ]
  where
    builtIn patch xs =
        map (\x -> patch x (Var.Canonical Var.BuiltIn x)) xs

    tuples =
        map (\n -> "_Tuple" ++ show n) [0 .. 9 :: Int]
