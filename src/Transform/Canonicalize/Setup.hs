{-# OPTIONS_GHC -Wall #-}
module Transform.Canonicalize.Setup (environment) where

import Control.Applicative ((<$>))
import qualified Data.Graph as Graph
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Traversable as Trav

import qualified AST.Declaration as D
import qualified AST.Expression.Valid as Valid
import qualified AST.Module as Module
import qualified AST.Pattern as P
import qualified AST.Type as Type
import qualified AST.Variable as Var
import Elm.Utils ((|>))
import qualified Transform.Canonicalize.Environment as Env
import qualified Transform.Canonicalize.Error as Error
import qualified Transform.Canonicalize.Result as Result
import qualified Transform.Canonicalize.Type as Canonicalize
import qualified Transform.Interface as Interface


environment
    :: Module.Interfaces
    -> Module.ValidModule
    -> Result.Result Env.Environment
environment interfaces modul@(Module.Module _ _ _ imports decls) =
  let moduleName =
          Module.names modul

      importPatchesResult =
          concat <$>
            Trav.traverse (importPatches interfaces) imports

      (typeAliasNodes, declPatches) =
          declarationsToPatches moduleName decls

      patches =
          (++) (Env.builtinPatches ++ declPatches) <$> importPatchesResult
  in
      (Env.fromPatches moduleName <$> patches)
          `Result.andThen` addTypeAliases moduleName typeAliasNodes


-- PATCHES FOR IMPORTS

importPatches
    :: Module.Interfaces
    -> (Module.Name, Module.ImportMethod)
    -> Result.Result [Env.Patch]
importPatches allInterfaces (importName, method) =
  case Interface.filterExports <$> Map.lookup importName allInterfaces of
    Nothing
        | Module.nameIsNative importName ->
            Result.ok []

        | otherwise ->
            allInterfaces
              |> Map.keys
              |> Error.nearbyNames Module.nameToString importName
              |> Error.moduleNotFound importName
              |> Result.err

    Just interface ->
        let (Module.ImportMethod maybeAlias listing) =
                method

            (Var.Listing exposedValues open) =
                listing

            qualifier =
                maybe (Module.nameToString importName) id maybeAlias

            qualifiedPatches =
                interfacePatches importName (qualifier ++ ".") interface

            unqualifiedPatches =
                if open
                  then Result.ok (interfacePatches importName "" interface)
                  else concat <$> Trav.traverse (valueToPatches importName interface) exposedValues
        in
            (++) qualifiedPatches <$> unqualifiedPatches


interfacePatches :: Module.Name -> String -> Module.Interface -> [Env.Patch]
interfacePatches moduleName prefix interface =
    let genericPatch mkPatch name value =
            mkPatch (prefix ++ name) value

        patch mkPatch name =
            genericPatch mkPatch name (Var.fromModule moduleName name)

        aliasPatch (name, (tvars, tipe)) =
            genericPatch Env.Alias name
                (Var.fromModule moduleName name, tvars, tipe)

        ctorNames =
            concatMap (map fst . snd) (Map.elems (Module.iAdts interface))
    in
        concat
          [ map (patch Env.Value) (Map.keys (Module.iTypes interface))
          , map (patch Env.Value) ctorNames
          , map (patch Env.Union) (Map.keys (Module.iAdts interface))
          , map aliasPatch (Map.toList (Module.iAliases interface))
          , map (patch Env.Pattern) ctorNames
          ]


-- PATCHES FOR INDIVIDUAL VALUES

valueToPatches
    :: Module.Name
    -> Module.Interface
    -> Var.Value
    -> Result.Result [Env.Patch]
valueToPatches moduleName interface value =
  let patch mkPatch x =
          mkPatch x (Var.fromModule moduleName x)

      notFound getNames x =
          Module.iExports interface
            |> getNames
            |> Error.nearbyNames id x
            |> Error.valueNotFound moduleName x
            |> Result.err
  in
  case value of
    Var.Value x
        | Map.notMember x (Module.iTypes interface) ->
            notFound Var.getValues x

        | otherwise ->
            Result.ok [patch Env.Value x]

    Var.Alias x ->
        case Map.lookup x (Module.iAliases interface) of
          Just (tvars, t) ->
              let alias =
                    Env.Alias x (Var.fromModule moduleName x, tvars, t)
              in
                  Result.ok $
                      if Map.member x (Module.iTypes interface)
                          then [alias, patch Env.Value x]
                          else [alias]

          Nothing ->
              case Map.lookup x (Module.iAdts interface) of
                Just (_,_) ->
                    Result.ok [patch Env.Union x]

                Nothing ->
                    let getNames values =
                            Var.getAliases values
                            ++ map fst (Var.getUnions values)
                    in
                        notFound getNames x

    Var.Union givenName (Var.Listing givenCtorNames open) ->
        case Map.lookup givenName (Module.iAdts interface) of
          Nothing ->
              notFound (map fst . Var.getUnions) givenName

          Just (_tvars, realCtors) ->
              patches <$>
                  if open
                    then Result.ok realCtorNames
                    else Trav.traverse ctorExists givenCtorNames
            where
              realCtorNames =
                  map fst realCtors

              ctorExists givenCtorName =
                  if givenCtorName `elem` realCtorNames
                    then Result.ok givenCtorName
                    else notFound (const realCtorNames) givenCtorName

              patches ctorNames =
                  patch Env.Union givenName
                  : map (patch Env.Value) ctorNames
                  ++ map (patch Env.Pattern) ctorNames


-- PATCHES FOR TYPE ALIASES

type Node = ((String, [String], Type.RawType), String, [String])


node :: String -> [String] -> Type.RawType -> Node
node name tvars alias =
    ((name, tvars, alias), name, edges alias)
  where
    edges tipe =
        case tipe of
          Type.Lambda t1 t2 ->
              edges t1 ++ edges t2

          Type.Var _ ->
              []

          Type.Type (Var.Raw x) ->
              [x]

          Type.App t ts ->
              edges t ++ concatMap edges ts

          Type.Record fs ext ->
              maybe [] edges ext ++ concatMap (edges . snd) fs

          Type.Aliased _ args aliasType ->
              case aliasType of
                Type.Holey t ->
                    edges t ++ concatMap (edges . snd) args
                Type.Filled t ->
                    edges t


addTypeAliases
    :: Module.Name
    -> [Node]
    -> Env.Environment
    -> Result.Result Env.Environment
addTypeAliases moduleName typeAliasNodes initialEnv =
    Result.foldl
        (addTypeAlias moduleName)
        initialEnv
        (Graph.stronglyConnComp typeAliasNodes)


addTypeAlias
    :: Module.Name
    -> Graph.SCC (String, [String], Type.RawType)
    -> Env.Environment
    -> Result.Result Env.Environment
addTypeAlias moduleName scc env =
  case scc of
    Graph.AcyclicSCC (name, tvars, alias) ->
        addToEnv <$> Canonicalize.tipe env alias
      where
        addToEnv alias' =
            let value =
                  (Var.fromModule moduleName name, tvars, alias')
            in
                env { Env._aliases = Env.insert name value (Env._aliases env) }

    Graph.CyclicSCC aliases ->
        Result.err (Error.recursiveAlias aliases)


-- DECLARATIONS TO PATCHES

declarationsToPatches
    :: Module.Name
    -> [D.ValidDecl]
    -> ([Node], [Env.Patch])
declarationsToPatches moduleName decls =
  let (maybeNodes, patchLists) =
          unzip (map (declToPatches moduleName) decls)
  in
      (Maybe.catMaybes maybeNodes, concat patchLists)


-- When canonicalizing, all _values should be Local, but all _adts and _patterns
-- should be fully namespaced. With _adts, they may appear in types that can
-- escape the module.
declToPatches
    :: Module.Name
    -> D.ValidDecl
    -> (Maybe Node, [Env.Patch])
declToPatches moduleName decl =
  let local mkPatch x =
          mkPatch x (Var.local x)

      namespaced mkPatch x =
          mkPatch x (Var.fromModule moduleName x)
  in
  case decl of
    D.Definition (Valid.Definition pattern _ _) ->
        ( Nothing
        , map (local Env.Value) (P.boundVarList pattern)
        )

    D.Datatype name _ ctors ->
        let ctorNames = map fst ctors
        in
            ( Nothing
            , namespaced Env.Union name
              : map (local Env.Value) ctorNames
              ++ map (namespaced Env.Pattern) ctorNames
            )

    D.TypeAlias name tvars alias ->
        ( Just (node name tvars alias)
        , case alias of
            Type.Record _ _ ->
                [local Env.Value name]
            _ ->
                []
        )

    D.Port port ->
        ( Nothing
        , [local Env.Value (D.validPortName port)]
        )

    D.Fixity _ _ _ ->
        ( Nothing
        , []
        )
