{-# OPTIONS_GHC -Wall #-}
module Canonicalize.Setup (environment) where

import Control.Applicative ((<$>))
import Control.Arrow (second)
import qualified Data.Graph as Graph
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Traversable as Trav

import qualified AST.Declaration as D
import qualified AST.Expression.Valid as Valid
import qualified AST.Module as Module
import qualified AST.Module.Name as ModuleName
import qualified AST.Pattern as P
import qualified AST.Type as Type
import qualified AST.Variable as Var
import Elm.Utils ((|>))
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Canonicalize as Error
import qualified Reporting.Error.Helpers as Error
import qualified Reporting.Region as R
import qualified Canonicalize.Environment as Env
import qualified Canonicalize.Result as Result
import qualified Canonicalize.Type as Canonicalize


environment
    :: Map.Map ModuleName.Raw ModuleName.Canonical
    -> Module.Interfaces
    -> Module.ValidModule
    -> Result.ResultErr Env.Environment
environment importDict interfaces (Module.Module name _ _ _ (defaults, imports) decls) =
  let
    allImports =
      imports ++ map (A.A (error "default import not found")) defaults

    importPatchesResult =
        concat <$> Trav.traverse (importPatches importDict interfaces) allImports

    (typeAliasNodes, declPatches) =
        declarationsToPatches name decls

    patches =
        (++) (Env.builtinPatches ++ declPatches) <$> importPatchesResult
  in
      (Env.fromPatches name <$> patches)
          `Result.andThen` addTypeAliases name typeAliasNodes


-- PATCHES FOR IMPORTS

importPatches
    :: Map.Map ModuleName.Raw ModuleName.Canonical
    -> Module.Interfaces
    -> A.Located (ModuleName.Raw, Module.ImportMethod)
    -> Result.ResultErr [Env.Patch]
importPatches importDict allInterfaces (A.A region (rawImportName, method)) =
  let
    maybeInterface =
      do  canonicalName <- Map.lookup rawImportName importDict
          interface <- Map.lookup canonicalName allInterfaces
          return (canonicalName, restrictToPublicApi interface)
  in
  case maybeInterface of
    Nothing ->
        if ModuleName.isNative rawImportName then
            Result.ok []

        else
            allInterfaces
              |> Map.keys
              |> map ModuleName._module
              |> Error.nearbyNames ModuleName.toString rawImportName
              |> Error.moduleNotFound rawImportName
              |> A.A region
              |> Result.err

    Just (importName, interface) ->
        let (Module.ImportMethod maybeAlias listing) =
                method

            (Var.Listing exposedValues open) =
                listing

            qualifier =
                maybe (ModuleName.toString rawImportName) id maybeAlias

            qualifiedPatches =
                interfacePatches importName (qualifier ++ ".") interface

            unqualifiedPatches =
                if open
                  then Result.ok (interfacePatches importName "" interface)
                  else concat <$> Trav.traverse (valueToPatches region importName interface) exposedValues
        in
            (++) qualifiedPatches <$> unqualifiedPatches


interfacePatches :: ModuleName.Canonical -> String -> Module.Interface -> [Env.Patch]
interfacePatches moduleName prefix interface =
    let genericPatch mkPatch name value =
            mkPatch (prefix ++ name) value

        patch mkPatch name =
            genericPatch mkPatch name (Var.fromModule moduleName name)

        aliasPatch (name, (tvars, tipe)) =
            genericPatch Env.Alias name
                (Var.fromModule moduleName name, tvars, tipe)

        patternPatch (name, args) =
            genericPatch Env.Pattern name
                (Var.fromModule moduleName name, length args)

        ctors =
            concatMap snd (Map.elems (Module.iAdts interface))

        ctorNames =
            map fst ctors
    in
        concat
          [ map (patch Env.Value) (Map.keys (Module.iTypes interface))
          , map (patch Env.Value) ctorNames
          , map (patch Env.Union) (Map.keys (Module.iAdts interface))
          , map aliasPatch (Map.toList (Module.iAliases interface))
          , map patternPatch ctors
          ]


-- PATCHES FOR INDIVIDUAL VALUES

valueToPatches
    :: R.Region
    -> ModuleName.Canonical
    -> Module.Interface
    -> Var.Value
    -> Result.ResultErr [Env.Patch]
valueToPatches region moduleName interface value =
  let patch mkPatch x =
          mkPatch x (Var.fromModule moduleName x)

      patternPatch (x, numArgs) =
          Env.Pattern x (Var.fromModule moduleName x, numArgs)

      notFound getNames x =
          Module.iExports interface
            |> getNames
            |> Error.nearbyNames id x
            |> Error.valueNotFound (ModuleName._module moduleName) x
            |> A.A region
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
                    then Result.ok realCtorList
                    else Trav.traverse ctorExists givenCtorNames
            where
              realCtorList =
                  map (second length) realCtors

              realCtorDict =
                  Map.fromList realCtorList

              ctorExists givenCtorName =
                  case Map.lookup givenCtorName realCtorDict of
                    Just numArgs ->
                        Result.ok (givenCtorName, numArgs)
                    Nothing ->
                        notFound (const (map fst realCtors)) givenCtorName

              patches ctors =
                  patch Env.Union givenName
                  : map (patch Env.Value . fst) ctors
                  ++ map patternPatch ctors


-- PATCHES FOR TYPE ALIASES

type Node = ((R.Region, String, [String], Type.Raw), String, [String])


node :: R.Region -> String -> [String] -> Type.Raw -> Node
node region name tvars alias =
    ((region, name, tvars, alias), name, edges alias)
  where
    edges (A.A _ tipe) =
        case tipe of
          Type.RLambda t1 t2 ->
              edges t1 ++ edges t2

          Type.RVar _ ->
              []

          Type.RType (Var.Raw x) ->
              [x]

          Type.RApp t ts ->
              edges t ++ concatMap edges ts

          Type.RRecord fs ext ->
              maybe [] edges ext ++ concatMap (edges . snd) fs


addTypeAliases
    :: ModuleName.Canonical
    -> [Node]
    -> Env.Environment
    -> Result.ResultErr Env.Environment
addTypeAliases moduleName typeAliasNodes initialEnv =
    Result.foldl
        (addTypeAlias moduleName)
        initialEnv
        (Graph.stronglyConnComp typeAliasNodes)


addTypeAlias
    :: ModuleName.Canonical
    -> Graph.SCC (R.Region, String, [String], Type.Raw)
    -> Env.Environment
    -> Result.ResultErr Env.Environment
addTypeAlias moduleName scc env =
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
        Result.err (A.A region (Error.Alias (Error.SelfRecursive name tvars alias)))

    Graph.CyclicSCC aliases@((region, _, _, _) : _) ->
        Result.err (A.A region (Error.Alias (Error.MutuallyRecursive aliases)))


-- DECLARATIONS TO PATCHES

declarationsToPatches
    :: ModuleName.Canonical
    -> [D.ValidDecl]
    -> ([Node], [Env.Patch])
declarationsToPatches moduleName decls =
  let (maybeNodes, patchLists) =
          unzip (map (declToPatches moduleName) decls)
  in
      (Maybe.catMaybes maybeNodes, concat patchLists)


-- When canonicalizing, it is important that:
--    _adts are fully namespaced (Var.Module ...)
--    _patterns are fully namespaced (Var.Module ...)
--    _values that are defined as top-level declarations are (Var.TopLevel ...)
--    all other _values are local (Var.Local)
declToPatches
    :: ModuleName.Canonical
    -> D.ValidDecl
    -> (Maybe Node, [Env.Patch])
declToPatches moduleName (A.A (region,_) decl) =
  let topLevel mkPatch x =
          mkPatch x (Var.topLevel moduleName x )

      namespaced mkPatch x =
          mkPatch x (Var.fromModule moduleName x)

      patternPatch (name, args) =
          Env.Pattern name (Var.fromModule moduleName name, length args)
  in
  case decl of
    D.Definition (Valid.Definition pattern _ _) ->
        ( Nothing
        , map (topLevel Env.Value) (P.boundVarList pattern)
        )

    D.Datatype name _ ctors ->
        let ctorNames = map fst ctors
        in
            ( Nothing
            , namespaced Env.Union name
              : map (topLevel Env.Value) ctorNames
              ++ map patternPatch ctors
            )

    D.TypeAlias name tvars alias ->
        ( Just (node region name tvars alias)
        , case alias of
            A.A _ (Type.RRecord _ _) ->
                [topLevel Env.Value name]
            _ ->
                []
        )

    D.Port port ->
        ( Nothing
        , [topLevel Env.Value (D.validPortName port)]
        )

    D.Fixity _ _ _ ->
        ( Nothing
        , []
        )


-- RESTRICT VISIBLE API OF INTERFACES

restrictToPublicApi :: Module.Interface -> Module.Interface
restrictToPublicApi interface =
    interface
    { Module.iTypes =
        Map.fromList $
          Maybe.mapMaybe
            (get (Module.iTypes interface))
            (Var.getValues exports ++ ctors)

    , Module.iAliases =
        Map.fromList $
          Maybe.mapMaybe
            (get (Module.iAliases interface))
            (Var.getAliases exports)

    , Module.iAdts =
        Map.fromList
          (Maybe.mapMaybe (trimUnions interface) unions)
    }
  where
    exports :: [Var.Value]
    exports =
        Module.iExports interface

    unions :: [(String, Var.Listing String)]
    unions =
        Var.getUnions exports

    ctors :: [String]
    ctors =
        concatMap (\(_, Var.Listing ctorList _) -> ctorList) unions


get :: Map.Map String a -> String -> Maybe (String, a)
get dict key =
  case Map.lookup key dict of
    Nothing -> Nothing
    Just value -> Just (key, value)


trimUnions
    :: Module.Interface
    -> (String, Var.Listing String)
    -> Maybe (String, Module.AdtInfo String)
trimUnions interface (name, Var.Listing exportedCtors _) =
  case Map.lookup name (Module.iAdts interface) of
    Nothing -> Nothing
    Just (tvars, ctors) ->
      let isExported (ctor, _) =
            ctor `elem` exportedCtors
      in
          Just (name, (tvars, filter isExported ctors))
