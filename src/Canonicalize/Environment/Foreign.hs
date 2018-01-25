{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Canonicalize.Environment.Foreign
  ( createInitialEnv
  )
  where


import Control.Monad (foldM)
import qualified Data.Map.Strict as Map

import qualified AST.Canonical as Can
import qualified AST.Source as Src
import qualified AST.Module.Name as ModuleName
import qualified Canonicalize.Environment as Env
import qualified Data.List as List
import qualified Data.OneOrMore as OneOrMore
import qualified Elm.Interface as I
import qualified Elm.Name as N
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Canonicalize as Error
import qualified Reporting.Helpers as Help (nearbyNames)
import qualified Reporting.Region as R
import qualified Reporting.Result as Result



-- RESULT


type Result i w a =
  Result.Result i w Error.Error a



-- CREATE ENVIRONMENT


type ImportDict =
  Map.Map N.Name ModuleName.Canonical


createInitialEnv :: ModuleName.Canonical -> ImportDict -> I.Interfaces -> [Src.Import] -> Result i w Env.Env
createInitialEnv home importDict interfaces sourceImports =
  do  imports <- traverse (verifyImport importDict interfaces) (filter isNotKernel sourceImports)
      (State vs ts cs bs qvs qts qcs) <- foldM addImport emptyState imports
      Result.ok (Env.Env home (Map.map Env.Foreign vs) ts cs bs qvs qts qcs)


isNotKernel :: Src.Import -> Bool
isNotKernel (Src.Import (A.At _ name) _ _) =
  not (ModuleName.isKernel name)



-- STATE


data State =
  State
    { _vars :: Env.Exposed Env.ForeignVar
    , _types :: Env.Exposed Env.Type
    , _ctors :: Env.Exposed Env.Ctor
    , _binops :: Env.Exposed Env.Binop
    , _q_vars :: Env.Qualified Env.ForeignVar
    , _q_types :: Env.Qualified Env.Type
    , _q_ctors :: Env.Qualified Env.Ctor
    }


emptyState :: State
emptyState =
  State Map.empty emptyTypes Map.empty Map.empty Map.empty Map.empty Map.empty


emptyTypes :: Env.Exposed Env.Type
emptyTypes =
  Map.singleton "List" (OneOrMore.one (Env.Union 1 ModuleName.list))



-- ADD IMPORTS


addImport :: State -> Import -> Result i w State
addImport (State vs ts cs bs qvs qts qcs) (Import home (I.Interface defs unions aliases binops) prefix exposing) =
  let
    !rawTypeInfo =
      Map.union
        (Map.mapWithKey (unionToType home) unions)
        (Map.mapWithKey (aliasToType home) aliases)

    !vars = Map.map (OneOrMore.one . Env.ForeignVar home) defs
    !types = Map.map (OneOrMore.one . fst) rawTypeInfo
    !ctors = Map.foldr (addExposed . snd) Map.empty rawTypeInfo

    !qvs2 = addQualified prefix vars qvs
    !qts2 = addQualified prefix types qts
    !qcs2 = addQualified prefix ctors qcs
  in
  case exposing of
    Src.Open ->
      let
        !vs2 = addExposed vs vars
        !ts2 = addExposed ts types
        !cs2 = addExposed cs ctors
        !bs2 = addExposed bs (Map.mapWithKey (binopToBinop home) binops)
      in
      Result.ok (State vs2 ts2 cs2 bs2 qvs2 qts2 qcs2)

    Src.Explicit exposedList ->
      foldM
        (addExposedValue home vars rawTypeInfo binops)
        (State vs ts cs bs qvs2 qts2 qcs2)
        exposedList


addExposed :: Env.Exposed a -> Env.Exposed a -> Env.Exposed a
addExposed =
  Map.unionWith OneOrMore.more


addQualified :: N.Name -> Env.Exposed a -> Env.Qualified a -> Env.Qualified a
addQualified prefix exposed qualified =
  Map.insertWith addExposed prefix exposed qualified



-- CANONICAL DATA TO ENVIRONMENT DATA


unionToType :: ModuleName.Canonical -> N.Name -> Can.Union -> (Env.Type, Env.Exposed Env.Ctor)
unionToType home name union@(Can.Union vars ctors _ _) =
  let
    addCtor dict (Can.Ctor ctor index _ args) =
      Map.insert ctor (OneOrMore.one (Env.Ctor home name union index args)) dict
  in
  ( Env.Union (length vars) home
  , List.foldl' addCtor Map.empty ctors
  )


aliasToType :: ModuleName.Canonical -> N.Name -> Can.Alias -> (Env.Type, Env.Exposed Env.Ctor)
aliasToType home name (Can.Alias vars tipe maybeRecordArgs) =
  (
    Env.Alias (length vars) home vars tipe
  ,
    case maybeRecordArgs of
      Nothing ->
        Map.empty

      Just _ ->
        Map.singleton name (OneOrMore.one (Env.RecordCtor home vars tipe))
  )


binopToBinop :: ModuleName.Canonical -> N.Name -> I.Binop -> OneOrMore.OneOrMore Env.Binop
binopToBinop home op (I.Binop name annotation associativity precedence) =
  OneOrMore.one (Env.Binop op home name annotation associativity precedence)



-- ADD EXPOSED VALUE


addExposedValue
  :: ModuleName.Canonical
  -> Env.Exposed Env.ForeignVar
  -> Map.Map N.Name (Env.Type, Env.Exposed Env.Ctor)
  -> Map.Map N.Name I.Binop
  -> State
  -> A.Located Src.Exposed
  -> Result i w State
addExposedValue home vars types binops (State vs ts cs bs qvs qts qcs) (A.At region exposed) =
  case exposed of
    Src.Lower name ->
      case Map.lookup name vars of
        Just info ->
          Result.ok (State (Map.insert name info vs) ts cs bs qvs qts qcs)

        Nothing ->
          throwExposingNotFound region name

    Src.Upper name privacy ->
      case privacy of
        Src.Private ->
          case Map.lookup name types of
            Just (tipe, ctors) ->
              case tipe of
                Env.Union _ _ ->
                  let
                    !ts2 = Map.insert name (OneOrMore.one tipe) ts
                  in
                  Result.ok (State vs ts2 cs bs qvs qts qcs)

                Env.Alias _ _ _ _ ->
                  let
                    !ts2 = Map.insert name (OneOrMore.one tipe) ts
                    !cs2 = addExposed cs ctors
                  in
                  Result.ok (State vs ts2 cs2 bs qvs qts qcs)

            Nothing ->
              throwExposingNotFound region name

        Src.Public ->
          case Map.lookup name types of
            Just (tipe, ctors) ->
              case tipe of
                Env.Union _ _ ->
                  let
                    !ts2 = Map.insert name (OneOrMore.one tipe) ts
                    !cs2 = addExposed cs ctors
                  in
                  Result.ok (State vs ts2 cs2 bs qvs qts qcs)

                Env.Alias _ _ _ _ ->
                  Result.throw (Error.ImportOpenAlias region name)

            Nothing ->
              throwExposingNotFound region name

    Src.Operator op ->
      case Map.lookup op binops of
        Just binop ->
          let
            !bs2 = Map.insert op (binopToBinop home op binop) bs
          in
          Result.ok (State vs ts cs bs2 qvs qts qcs)

        Nothing ->
          throwExposingNotFound region op



-- VERIFY IMPORT


data Import =
  Import
    { _name :: ModuleName.Canonical
    , _interface :: I.Interface
    , _prefix :: N.Name
    , _exposing :: Src.Exposing
    }


verifyImport :: ImportDict -> I.Interfaces -> Src.Import -> Result i w Import
verifyImport importDict interfaces (Src.Import (A.At region name) alias exposing) =
  case Map.lookup name importDict of
    Just canonicalName ->
      case Map.lookup canonicalName interfaces of
        Just interface ->
          Result.ok (Import canonicalName interface (maybe name id alias) exposing)

        Nothing ->
          throwImportNotFound region name (Map.keys interfaces)

    Nothing ->
      throwImportNotFound region name (Map.elems importDict)


throwImportNotFound :: R.Region -> N.Name -> [ModuleName.Canonical] -> Result i w a
throwImportNotFound region name knownModules =
  Result.throw $ Error.ImportNotFound region name $
    Help.nearbyNames N.toString name $
      map ModuleName._module knownModules


throwExposingNotFound :: R.Region -> N.Name -> Result i w a
throwExposingNotFound region name =
  Result.throw (error ("TODO throwExposingNotFound " ++ N.toString name) region name)

{-
  let
    ctorDict =
      Map.fromList (concatMap toCtorPairs (Map.toList unions))

    toCtorPairs (tipe, union) =
      case union of
        Can.Union _ _ ctors ->
          map (\(Can.Ctor ctor _ _) -> (ctor, tipe)) ctors

        Can.Enum _ _ names ->
          map (\ctor -> (ctor, tipe)) names

        Can.Box _ ctor _ ->
          [(ctor, tipe)]
  in
  case Map.lookup name ctorDict of
    Just tipe ->
      Result.throw $ Error.ImportCtorByName region name tipe

    Nothing ->
      Result.throw $ Error.ImportExposingNotFound region moduleName name $
        Help.nearbyNames N.toString name $ Set.toList $
          Set.unions [ Map.keysSet types, Map.keysSet unions, Map.keysSet aliases ]
-}