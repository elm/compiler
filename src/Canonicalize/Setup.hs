{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Canonicalize.Setup (environment) where

import Control.Arrow (second)
import Control.Monad (foldM)
import qualified Data.Graph as Graph
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Monoid ((<>))
import Data.Text (Text)

import qualified AST.Declaration as D
import qualified AST.Effects as Effects
import qualified AST.Exposing as Exposing
import qualified AST.Expression.Source as Src
import qualified AST.Module as Module
import qualified AST.Module.Name as ModuleName
import qualified AST.Pattern as P
import qualified AST.Type as Type
import qualified AST.Variable as Var
import Elm.Utils ((|>))
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Canonicalize as Error
import qualified Reporting.Helpers as Help (nearbyNames)
import qualified Reporting.Region as R
import qualified Reporting.Result as Result
import qualified Canonicalize.Environment as Env
import qualified Canonicalize.Type as Canonicalize
import Canonicalize.Variable (Result)



environment
    :: Map.Map ModuleName.Raw ModuleName.Canonical
    -> Module.Interfaces
    -> Module.Valid
    -> Result Env.Env
environment importDict interfaces (Module.Module name info) =
  let
    (Module.Valid _ _ (defaults, imports) decls effects) =
      info

    allImports =
      imports ++ map toPretendUserImport defaults

    getImportPatches =
      traverse (importToPatches importDict interfaces) allImports

    (typeAliasNodes, declPatches) =
      declsToPatches name decls

    effectPatches =
      effectsToPatches name effects

    createEnv importPatches =
      Env.fromPatches
        name
        (concat (Env.builtinPatches : declPatches : effectPatches : importPatches))
  in
    do  env <- createEnv <$> getImportPatches
        addTypeAliases name typeAliasNodes env


toPretendUserImport :: Module.DefaultImport -> Module.UserImport
toPretendUserImport (name, method) =
  let
    errLoc =
      error "default import not found"
  in
    A.A errLoc ( A.A errLoc name, method )



-- PATCHES FOR IMPORTS


importToPatches
  :: Map.Map ModuleName.Raw ModuleName.Canonical
  -> Module.Interfaces
  -> Module.UserImport
  -> Result [Env.Patch]
importToPatches importDict allInterfaces (A.A _ (A.A region rawImportName, method)) =
  let
    maybeInterface =
      do  canonicalName <- Map.lookup rawImportName importDict
          interface <- Map.lookup canonicalName allInterfaces
          return (canonicalName, restrictToPublicApi interface)
  in
  case maybeInterface of
    Nothing ->
      if ModuleName.isKernel rawImportName then
        Result.ok []

      else
        allInterfaces
          |> Map.keys
          |> map ModuleName._module
          |> Help.nearbyNames ModuleName.toText rawImportName
          |> Error.moduleNotFound rawImportName
          |> Result.throw region

    Just (importName, interface) ->
      let
        (Module.ImportMethod maybeAlias listing) =
          method

        qualifier =
          maybe (ModuleName.toText rawImportName) id maybeAlias

        infixPatches =
          map (infixToPatch (Var.fromModule importName)) (Module.iFixities interface)

        qualifiedPatches =
          interfaceToPatches importName (qualifier <> ".") interface

        unqualifiedPatches =
          case listing of
            Exposing.Open ->
              Result.ok (interfaceToPatches importName "" interface)

            Exposing.Explicit xs ->
              concat <$> traverse (entryToPatches importName interface) xs
      in
        (++) (infixPatches ++ qualifiedPatches) <$> unqualifiedPatches


interfaceToPatches :: ModuleName.Canonical -> Text -> Module.Interface -> [Env.Patch]
interfaceToPatches moduleName prefix interface =
  let
    genericPatch mkPatch name value =
      mkPatch (prefix <> name) value

    patch mkPatch name =
      genericPatch mkPatch name (Var.fromModule moduleName name)

    aliasPatch (name, (tvars, tipe)) =
      genericPatch
        Env.Alias
        name
        (Var.fromModule moduleName name, tvars, tipe)

    patternPatch (name, args) =
      genericPatch
        Env.Pattern
        name
        (Var.fromModule moduleName name, length args)

    ctors =
      concatMap snd (Map.elems (Module.iUnions interface))

    ctorNames =
      map fst ctors
  in
    concat
      [ map (patch Env.Value) (Map.keys (Module.iTypes interface))
      , map (patch Env.Value) ctorNames
      , map (patch Env.Union) (Map.keys (Module.iUnions interface))
      , map aliasPatch (Map.toList (Module.iAliases interface))
      , map patternPatch ctors
      ]



-- PATCHES FOR INDIVIDUAL ENTRIES


entryToPatches
  :: ModuleName.Canonical
  -> Module.Interface
  -> A.Located Exposing.Entry
  -> Result [Env.Patch]
entryToPatches moduleName interface (A.A region entry) =
  let
    patch mkPatch name =
      mkPatch name (Var.fromModule moduleName name)

    patternPatch (name, numArgs) =
      Env.Pattern name (Var.fromModule moduleName name, numArgs)

    notFound subRegion getNames name =
      Module.iExports interface
        |> getNames
        |> Help.nearbyNames id name
        |> Error.valueNotFound (ModuleName._module moduleName) name
        |> Result.throw subRegion
  in
  case entry of
    Exposing.Lower name ->
      if Map.notMember name (Module.iTypes interface) then
          notFound region Exposing._values name

      else
          Result.ok [patch Env.Value name]

    Exposing.Upper x Nothing ->
      case Map.lookup x (Module.iAliases interface) of
        Just (tvars, t) ->
          let
            alias =
              Env.Alias x (Var.fromModule moduleName x, tvars, t)
          in
            Result.ok $
              if Map.member x (Module.iTypes interface) then
                [alias, patch Env.Value x]

              else
                [alias]

        Nothing ->
          case Map.lookup x (Module.iUnions interface) of
            Just (_,_) ->
              Result.ok [patch Env.Union x]

            Nothing ->
              let getNames values =
                    Exposing._aliases values
                    ++ map fst (Exposing._unions values)
              in
                notFound region getNames x

    Exposing.Upper givenName (Just exposing) ->
      case Map.lookup givenName (Module.iUnions interface) of
        Nothing ->
          notFound region (map fst . Exposing._unions) givenName

        Just (_tvars, realCtors) ->
          patches <$>
            case exposing of
              Exposing.Open ->
                Result.ok realCtorList

              Exposing.Explicit xs ->
                traverse ctorExists xs
          where
            realCtorList =
                map (second length) realCtors

            realCtorDict =
                Map.fromList realCtorList

            ctorExists (A.A ctorRegion givenCtorName) =
                case Map.lookup givenCtorName realCtorDict of
                  Just numArgs ->
                    Result.ok (givenCtorName, numArgs)

                  Nothing ->
                    notFound ctorRegion (const (map fst realCtors)) givenCtorName

            patches ctors =
                patch Env.Union givenName
                : map (patch Env.Value . fst) ctors
                ++ map patternPatch ctors



-- PATCHES FOR TYPE ALIASES


type Node =
  ( (R.Region, Text, [Text], Type.Raw), Text, [Text] )


node :: R.Region -> Text -> [Text] -> Type.Raw -> Node
node region name tvars alias =
    ((region, name, tvars, alias), name, edges alias)
  where
    edges (A.A _ tipe) =
      case tipe of
        Type.RLambda t1 t2 ->
          edges t1 ++ edges t2

        Type.RVar _ ->
          []

        Type.RType (A.A _ (Var.Raw x)) args ->
          x : concatMap edges args

        Type.RRecord fs ext ->
          maybe [] edges ext ++ concatMap (edges . snd) fs


addTypeAliases :: ModuleName.Canonical -> [Node] -> Env.Env -> Result Env.Env
addTypeAliases moduleName typeAliasNodes initialEnv =
  foldM (addTypeAlias moduleName) initialEnv (Graph.stronglyConnComp typeAliasNodes)


addTypeAlias
    :: ModuleName.Canonical
    -> Env.Env
    -> Graph.SCC (R.Region, Text, [Text], Type.Raw)
    -> Result Env.Env
addTypeAlias moduleName env scc =
  case scc of
    Graph.AcyclicSCC (_, name, tvars, alias) ->
        addToEnv <$> Canonicalize.tipe env alias
      where
        addToEnv alias' =
            let value =
                  (Var.fromModule moduleName name, tvars, alias')
            in
                env { Env._aliases = Env.insert name value (Env._aliases env) }

    Graph.CyclicSCC [] ->
      Result.ok env

    Graph.CyclicSCC [(region, name, tvars, alias)] ->
      Result.throw region (Error.Alias (Error.SelfRecursive name tvars alias))

    Graph.CyclicSCC aliases@((region, _, _, _) : _) ->
      Result.throw region (Error.Alias (Error.MutuallyRecursive aliases))



-- DECLARATIONS TO PATCHES


{- When canonicalizing, it is important that:

    _adts are fully namespaced (Var.Module ...)
    _patterns are fully namespaced (Var.Module ...)
    _values that are defined as top-level declarations are (Var.TopLevel ...)
    all other _values are local (Var.Local)

-}
declsToPatches :: ModuleName.Canonical -> D.Valid -> ([Node], [Env.Patch])
declsToPatches moduleName (D.Decls defs unions aliases infixes) =
  let

    -- HELPERS

    topLevelValue x =
      Env.Value x (Var.topLevel moduleName x)

    patternPatch (name, args) =
      Env.Pattern name (Var.fromModule moduleName name, length args)

    -- TO PATCHES

    defToPatches (A.A _ (Src.Def _ pattern _ _)) =
      map topLevelValue (P.boundVarList pattern)

    unionToPatches (A.A _ (D.Type name _ ctors)) =
      Env.Union name (Var.fromModule moduleName name)
      :  map topLevelValue (map fst ctors)
      ++ map patternPatch ctors

    aliasToPatches (A.A _ (D.Type name _ alias)) =
      case A.drop alias of
        Type.RRecord _ Nothing ->
            Just (topLevelValue name)

        _ ->
            Nothing

    -- TO NODES

    aliasToNode (A.A (region, _) (D.Type name tvars alias)) =
      node region name tvars alias
  in
    (
      map aliasToNode aliases
    ,
      concat
        [ map (infixToPatch (Var.topLevel moduleName)) infixes
        , Maybe.mapMaybe aliasToPatches aliases
        , concatMap unionToPatches unions
        , concatMap defToPatches defs
        ]
    )


infixToPatch :: (Text -> Var.Canonical) -> D.Infix -> Env.Patch
infixToPatch toVar (D.Infix name assoc prec) =
  Env.Infix (toVar name) assoc prec



-- EFFECTS TO PATCHES


effectsToPatches :: ModuleName.Canonical -> Effects.Raw -> [Env.Patch]
effectsToPatches moduleName effects =
  case effects of
    Effects.None ->
      []

    Effects.Port ports ->
      let
        toPatch (A.A _ (Effects.PortRaw name _)) =
          Env.Value name (Var.topLevel moduleName name)
      in
        map toPatch ports

    Effects.Manager _ info ->
      map (\name -> Env.Value name (Var.topLevel moduleName name)) $
        case Effects._managerType info of
          Effects.CmdManager _ ->
            [ "command" ]

          Effects.SubManager _ ->
            [ "subscription" ]

          Effects.FxManager _ _ ->
            [ "command", "subscription" ]



-- RESTRICT VISIBLE API OF INTERFACES


restrictToPublicApi :: Module.Interface -> Module.Interface
restrictToPublicApi interface =
  let
    (Exposing.Canonical values aliases unions) =
      Module.iExports interface
  in
    interface
      { Module.iTypes = Map.fromList $
          Maybe.mapMaybe (get (Module.iTypes interface)) (values ++ concatMap snd unions)

      , Module.iAliases = Map.fromList $
          Maybe.mapMaybe (get (Module.iAliases interface)) aliases

      , Module.iUnions = Map.fromList $
          Maybe.mapMaybe (trimUnions interface) unions
      }



get :: Map.Map Text a -> Text -> Maybe (Text, a)
get dict key =
  case Map.lookup key dict of
    Nothing ->
      Nothing

    Just value ->
      Just (key, value)


trimUnions :: Module.Interface -> (Text, [Text]) -> Maybe (Text, Module.UnionInfo Text)
trimUnions interface (name, exposedCtors) =
  case Map.lookup name (Module.iUnions interface) of
    Nothing ->
      Nothing

    Just (tvars, ctors) ->
      let
        isExposed (ctor, _) =
          ctor `elem` exposedCtors
      in
        Just (name, (tvars, filter isExposed ctors))
