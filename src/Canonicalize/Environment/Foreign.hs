{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Canonicalize.Environment.Foreign
  ( createInitialEnv
  )
  where


import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import qualified AST.Canonical as Can
import qualified AST.Source as Src
import qualified AST.Module.Name as ModuleName
import qualified Canonicalize.Environment.Internals as Env
import qualified Data.Bag as Bag
import qualified Data.OneOrMore as OneOrMore
import qualified Elm.Interface as I
import qualified Elm.Name as N
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Canonicalize as Error
import qualified Reporting.Helpers as Help (nearbyNames)
import qualified Reporting.Region as R
import qualified Reporting.Result as Result
import qualified Reporting.Warning as Warning



-- RESULT


type Result a =
  Result.Result () Warning.Warning Error.Error a



-- ENVIRONMENT


data Env =
  Env
    { _vars :: Map.Map N.Name (Env.Homes Env.ForeignVarHome)
    , _types :: Map.Map N.Name (Env.Homes Env.Type)
    , _patterns :: Map.Map N.Name (Env.Homes Env.Pattern)
    , _binops :: Map.Map N.Name (OneOrMore.OneOrMore Env.Binop)
    }



-- MERGE


merge :: Env -> Env -> Env
merge (Env v1 t1 p1 b1) (Env v2 t2 p2 b2) =
  Env
    (Map.unionWith mergeHomes v1 v2)
    (Map.unionWith mergeHomes t1 t2)
    (Map.unionWith mergeHomes p1 p2)
    (Map.unionWith OneOrMore.more b1 b2)


mergeHomes :: Env.Homes a -> Env.Homes a -> Env.Homes a
mergeHomes (Env.Homes unqualified1 qualified1) (Env.Homes unqualified2 qualified2) =
  Env.Homes
    (Bag.append unqualified1 unqualified2)
    (Map.unionWith OneOrMore.more qualified1 qualified2)


emptyEnv :: Env
emptyEnv =
  Env Map.empty Map.empty Map.empty Map.empty



-- CREATE ENVIRONMENT


type ImportDict =
  Map.Map N.Name ModuleName.Canonical


createInitialEnv :: ModuleName.Canonical -> ImportDict -> I.Interfaces -> [Src.Import] -> Result Env.Env
createInitialEnv home importDict interfaces sourceImports =
  do  imports <- traverse (verifyImport importDict interfaces) sourceImports
      let (Env vars types patterns binops) = foldr merge emptyEnv (map importToEnv imports)
      let toVarHome (Env.Homes u q) = Env.VarHomes (Env.Foreign u) q
      return $ Env.Env home (Map.map toVarHome vars) types patterns binops


importToEnv :: Import -> Env
importToEnv (Import home (I.Interface types unions aliases binops) prefix exposing) =
  let
    patterns =
      Map.fromList (concatMap (toPatterns home) (Map.toList unions))
  in
  case exposing of
    Open ->
      Env
        (Map.map (toOpenHomes home prefix toVarInfo) types)
        (Map.unionWith mergeHomes
          (Map.map (toOpenHomes home prefix toUnionInfo) unions)
          (Map.map (toOpenHomes home prefix toAliasInfo) aliases)
        )
        (Map.map (toOpenHomes home prefix toPatternInfo) patterns)
        (Map.mapWithKey (toBinopHomes home) binops)

    Explicit explicits ->
      Env
        (Map.mapWithKey (toExplicitHomes home prefix explicits toVarInfo toVarBag) types)
        (Map.unionWith mergeHomes
          (Map.mapWithKey (toExplicitHomes home prefix explicits toUnionInfo toUnionBag) unions)
          (Map.mapWithKey (toExplicitHomes home prefix explicits toAliasInfo toAliasBag) aliases)
        )
        (Map.mapWithKey (toExplicitHomes home prefix explicits toPatternInfo toPatternBag) patterns)
        (Map.mapWithKey (toBinopHomes home) binops)


toOpenHomes
  :: ModuleName.Canonical
  -> N.Name
  -> (ModuleName.Canonical -> value -> info)
  -> value
  -> Env.Homes info
toOpenHomes home prefix toInfo value =
  let info = toInfo home value in
  Env.Homes
    (Bag.one info)
    (Map.singleton prefix (OneOrMore.one info))


toExplicitHomes
  :: ModuleName.Canonical
  -> N.Name
  -> Map.Map N.Name Exposed
  -> (ModuleName.Canonical -> value -> info)
  -> (info -> Maybe Exposed -> Bag.Bag info)
  -> N.Name
  -> value
  -> Env.Homes info
toExplicitHomes home prefix explicits toInfo toBag name value =
  let info = toInfo home value in
  Env.Homes
    (toBag info (Map.lookup name explicits))
    (Map.singleton prefix (OneOrMore.one info))


toBinopHomes :: ModuleName.Canonical -> N.Name -> I.Op -> OneOrMore.OneOrMore Env.Binop
toBinopHomes home op (I.Op name annotation associativity precedence) =
  OneOrMore.one (Env.Binop op home name annotation associativity precedence)



-- TO INFO


{-# INLINE toVarInfo #-}
toVarInfo :: ModuleName.Canonical -> Can.Annotation -> Env.ForeignVarHome
toVarInfo =
  Env.ForeignVarHome


toUnionInfo :: ModuleName.Canonical -> Can.Union -> Env.Type
toUnionInfo home (Can.Union args _) =
  Env.Union (length args) home


toAliasInfo :: ModuleName.Canonical -> Can.Alias -> Env.Type
toAliasInfo home (Can.Alias args tipe _) =
  Env.Alias (length args) home args tipe


toPatternInfo :: ModuleName.Canonical -> Env.Pattern -> Env.Pattern
toPatternInfo _ pattern =
  pattern


toPatterns :: ModuleName.Canonical -> (N.Name, Can.Union) -> [(N.Name, Env.Pattern)]
toPatterns home (tipe, Can.Union vars ctors) =
  let
    fromCtor (name, args) =
      (name, Env.Pattern home tipe vars args)
  in
  map fromCtor ctors



-- TO BAG


toVarBag :: a -> Maybe Exposed -> Bag.Bag a
toVarBag info maybeExposed =
  case maybeExposed of
    Nothing ->
      Bag.empty

    Just exposed ->
      case exposed of
        Value ->
          Bag.one info

        Type ->
          Bag.empty

        Ctor Src.Private ->
          Bag.empty

        Ctor Src.Public ->
          Bag.one info

        Alias ->
          Bag.one info


toUnionBag :: a -> Maybe Exposed -> Bag.Bag a
toUnionBag info maybeExposed =
  case maybeExposed of
    Just Type ->
      Bag.one info

    _ ->
      Bag.empty


toAliasBag :: a -> Maybe Exposed -> Bag.Bag a
toAliasBag info maybeExposed =
  case maybeExposed of
    Just Alias ->
      Bag.one info

    _ ->
      Bag.empty


toPatternBag :: a -> Maybe Exposed -> Bag.Bag a
toPatternBag info maybeExposed =
  case maybeExposed of
    Just (Ctor Src.Public) ->
      Bag.one info

    _ ->
      Bag.empty



-- VERIFY IMPORT


data Import =
  Import
    { _name :: ModuleName.Canonical
    , _interface :: I.Interface
    , _prefix :: N.Name
    , _exposing :: Exposing
    }


data Exposing
  = Open
  | Explicit (Map.Map N.Name Exposed)


data Exposed
  = Value
  | Type
  | Ctor Src.Privacy
  | Alias


verifyImport :: ImportDict -> I.Interfaces -> Src.Import -> Result Import
verifyImport importDict interfaces (Src.Import (A.At region name) alias exposing) =
  case Map.lookup name importDict of
    Nothing ->
      throwImportNotFound region name (Map.elems importDict)

    Just canonicalName ->
      case Map.lookup canonicalName interfaces of
        Nothing ->
          throwImportNotFound region name (Map.keys interfaces)

        Just interface ->
          Import canonicalName interface (maybe name id alias) <$>
            case exposing of
              Src.Open ->
                Result.ok Open

              Src.Explicit exposedList ->
                do  pairLists <- traverse (verifyExposed name interface) exposedList
                    Result.ok (Explicit (Map.fromList (concat pairLists)))


throwImportNotFound :: R.Region -> N.Name -> [ModuleName.Canonical] -> Result a
throwImportNotFound region name knownModules =
  Result.throw region $ Error.ImportNotFound name $
    Help.nearbyNames N.toString name $
      map ModuleName._module knownModules


verifyExposed :: N.Name -> I.Interface -> A.Located Src.Exposed -> Result [(N.Name, Exposed)]
verifyExposed moduleName interface@(I.Interface _ unions aliases _) (A.At region exposed) =
  case exposed of
    Src.Operator name ->
      verifyValue moduleName region name interface

    Src.Lower name ->
      verifyValue moduleName region name interface

    Src.Upper name privacy ->
      case Map.lookup name unions of
        Just (Can.Union _ ctors) ->
          let repair (ctor, _) = (ctor, Ctor privacy) in
          Result.ok $ (name, Type) : map repair ctors

        Nothing ->
          case Map.lookup name aliases of
            Just _ ->
              verifyAlias region name privacy

            Nothing ->
              throwExposingNotFound moduleName region name interface


verifyValue :: N.Name -> R.Region -> N.Name -> I.Interface -> Result [(N.Name, Exposed)]
verifyValue moduleName region name interface@(I.Interface types _ _ _) =
  case Map.lookup name types of
    Nothing ->
      throwExposingNotFound moduleName region name interface

    Just _ ->
      Result.ok [(name, Value)]


verifyAlias :: R.Region -> N.Name -> Src.Privacy -> Result [(N.Name, Exposed)]
verifyAlias region name privacy =
  case privacy of
    Src.Public ->
      Result.throw region (error "TODO cannot have (..) on type alias")

    Src.Private ->
      Result.ok [(name, Alias)]


throwExposingNotFound :: N.Name -> R.Region -> N.Name -> I.Interface -> Result a
throwExposingNotFound moduleName region name (I.Interface types unions aliases _) =
  let
    toCtorPairs (tipe, Can.Union _ ctors) =
      map (\(ctor, _) -> (ctor, tipe)) ctors

    ctorDict =
      Map.fromList (concatMap toCtorPairs (Map.toList unions))
  in
  case Map.lookup name ctorDict of
    Just tipe ->
      Result.throw region $ Error.ImportCtorNotFound name tipe

    Nothing ->
      Result.throw region $ Error.ImportExposingNotFound moduleName name $
        Help.nearbyNames N.toString name $ Set.toList $
          Set.unions [ Map.keysSet types, Map.keysSet unions, Map.keysSet aliases ]
