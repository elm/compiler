{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Canonicalize.Environment
  ( Env(..)
  , getPackage
  , insert
  , Patch(..), fromPatches, addPattern
  , builtinPatches
  , toLocalizer
  )
  where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Text (Text)

import qualified AST.Helpers as Help
import qualified AST.Declaration as Decl
import qualified AST.Module.Name as ModuleName
import qualified AST.Pattern as P
import qualified AST.Type as Type
import qualified AST.Variable as Var
import qualified Elm.Package as Pkg
import qualified Reporting.Render.Type as RT



-- ENVIRONMENT


data Env = Env
    { _home     :: ModuleName.Canonical
    , _values   :: Dict Var.Canonical
    , _unions   :: Dict Var.Canonical
    , _aliases  :: Dict (Var.Canonical, [Text], Type.Canonical)
    , _patterns :: Dict (Var.Canonical, Int)
    , _infixes  :: Map.Map Var.Canonical (Decl.Assoc, Int)
    }


type Dict a =
    Map.Map Text (Set.Set a)


fromPatches :: ModuleName.Canonical -> [Patch] -> Env
fromPatches moduleName patches =
    addPatches patches (Env moduleName Map.empty Map.empty Map.empty Map.empty Map.empty)


addPattern :: P.Pattern ann var -> Env -> Env
addPattern pattern env =
  let
    patches =
      map (\x -> Value x (Var.local x)) (P.boundVarList pattern)
  in
    addPatches patches env


getPackage :: Env -> Pkg.Name
getPackage env =
  ModuleName._package (_home env)



-- PATCHES


data Patch
    = Value Text Var.Canonical
    | Union Text Var.Canonical
    | Alias Text (Var.Canonical, [Text], Type.Canonical)
    | Pattern Text (Var.Canonical, Int)
    | Infix Var.Canonical Decl.Assoc Int



-- ADD PATCH TO ENVIRONMENT


addPatches :: [Patch] -> Env -> Env
addPatches patches env =
  List.foldl' (flip addPatch) env patches


addPatch :: Patch -> Env -> Env
addPatch patch env =
  case patch of
    Value name var ->
        env { _values = insert name var (_values env) }

    Union name var ->
        env { _unions = insert name var (_unions env) }

    Alias name var ->
        env { _aliases = insert name var (_aliases env) }

    Pattern name var ->
        env { _patterns = insert name var (_patterns env) }

    Infix name assoc prec ->
        env { _infixes = Map.insert name (assoc, prec) (_infixes env) }


insert :: (Ord a) => Text -> a -> Dict a -> Dict a
insert key value =
  Map.insertWith Set.union key (Set.singleton value)



-- PATCH HELPERS


builtinPatches :: [Patch]
builtinPatches =
    valuePatches ++ unionPatches ++ patternPatches
  where
    validTupleSizes =
      [0..9]

    valuePatches :: [Patch]
    valuePatches =
      map (\n -> Value (Help.makeTuple n) (Var.tuple n)) validTupleSizes

    unionPatches :: [Patch]
    unionPatches =
      [ Union "List" Var.list
      , Union "Int" Var.int
      , Union "Float" Var.float
      , Union "Char" Var.char
      , Union "Bool" Var.bool
      , Union "String" Var.string
      ]
      ++ map (\n -> Union (Help.makeTuple n) (Var.tuple n)) validTupleSizes

    patternPatches :: [Patch]
    patternPatches =
      [ Pattern "::" (Var.cons, 2)
      , Pattern "[]" (Var.nil, 0)
      ]
      ++ map (\n -> Pattern (Help.makeTuple n) (Var.tuple n, n)) validTupleSizes



-- TO TYPE DEALIASER


toLocalizer :: Env -> RT.Localizer
toLocalizer (Env _ _ unions aliases _ _) =
  let
    dealiasUnion (localName, canonicalSet) =
      case Set.toList canonicalSet of
        [canonicalName] ->
            Just (canonicalName, localName)
        _ ->
            Nothing

    dealiasAlias (localName, canonicalSet) =
      case Set.toList canonicalSet of
        [(canonicalName,_,_)] ->
            Just (canonicalName, localName)
        _ ->
            Nothing

    unionPairs =
      Maybe.mapMaybe dealiasUnion (Map.toList unions)

    aliasPairs =
      Maybe.mapMaybe dealiasAlias (Map.toList aliases)

    add (key,value) dict =
      Map.insertWith (\v v' -> if Text.length v < Text.length v' then v else v') key value dict
  in
    foldr add Map.empty (unionPairs ++ aliasPairs)
