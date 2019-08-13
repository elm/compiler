{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Canonicalize.Environment.Foreign
  ( createInitialEnv
  )
  where


import Control.Monad (foldM)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!))
import qualified Data.Name as Name

import qualified AST.Canonical as Can
import qualified AST.Source as Src
import qualified Canonicalize.Environment as Env
import qualified Elm.Interface as I
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Canonicalize as Error
import qualified Reporting.Result as Result



-- RESULT


type Result i w a =
  Result.Result i w Error.Error a


createInitialEnv :: ModuleName.Canonical -> Map.Map ModuleName.Raw I.Interface -> [Src.Import] -> Result i w Env.Env
createInitialEnv home ifaces imports =
  do  (State vs ts cs bs qvs qts qcs) <- foldM (addImport ifaces) emptyState (toSafeImports home imports)
      Result.ok (Env.Env home (Map.map Env.Foreign vs) ts cs bs qvs qts qcs)



-- STATE


data State =
  State
    { _vars :: Env.Exposed Can.Annotation
    , _types :: Env.Exposed Env.Type
    , _ctors :: Env.Exposed Env.Ctor
    , _binops :: Env.Exposed Env.Binop
    , _q_vars :: Env.Qualified Can.Annotation
    , _q_types :: Env.Qualified Env.Type
    , _q_ctors :: Env.Qualified Env.Ctor
    }


emptyState :: State
emptyState =
  State Map.empty emptyTypes Map.empty Map.empty Map.empty Map.empty Map.empty


emptyTypes :: Env.Exposed Env.Type
emptyTypes =
  Map.singleton "List" (Map.singleton ModuleName.list (Env.Union 1 ModuleName.list))



-- TO SAFE IMPORTS


toSafeImports :: ModuleName.Canonical -> [Src.Import] -> [Src.Import]
toSafeImports (ModuleName.Canonical pkg _) imports =
  if Pkg.isKernel pkg
  then filter isNormal imports
  else imports


isNormal :: Src.Import -> Bool
isNormal (Src.Import (A.At _ name) maybeAlias _) =
  if Name.isKernel name
  then
    case maybeAlias of
      Nothing -> False
      Just _ -> error "kernel imports cannot use `as`"
  else
    True



-- ADD IMPORTS


addImport :: Map.Map ModuleName.Raw I.Interface -> State -> Src.Import -> Result i w State
addImport ifaces (State vs ts cs bs qvs qts qcs) (Src.Import (A.At _ name) maybeAlias exposing) =
  let
    (I.Interface pkg defs unions aliases binops) = ifaces ! name
    !prefix = maybe name id maybeAlias
    !home = ModuleName.Canonical pkg name

    !rawTypeInfo =
      Map.union
        (Map.mapMaybeWithKey (unionToType home) unions)
        (Map.mapMaybeWithKey (aliasToType home) aliases)

    !vars = Map.map (Map.singleton home) defs
    !types = Map.map (Map.singleton home . fst) rawTypeInfo
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
  Map.unionWith Map.union


addQualified :: Name.Name -> Env.Exposed a -> Env.Qualified a -> Env.Qualified a
addQualified prefix exposed qualified =
  Map.insertWith addExposed prefix exposed qualified



-- UNION


unionToType :: ModuleName.Canonical -> Name.Name -> I.Union -> Maybe (Env.Type, Env.Exposed Env.Ctor)
unionToType home name union =
  unionToTypeHelp home name <$> I.toPublicUnion union


unionToTypeHelp :: ModuleName.Canonical -> Name.Name -> Can.Union -> (Env.Type, Env.Exposed Env.Ctor)
unionToTypeHelp home name union@(Can.Union vars ctors _ _) =
  let
    addCtor dict (Can.Ctor ctor index _ args) =
      Map.insert ctor (Map.singleton home (Env.Ctor home name union index args)) dict
  in
  ( Env.Union (length vars) home
  , List.foldl' addCtor Map.empty ctors
  )



-- ALIAS


aliasToType :: ModuleName.Canonical -> Name.Name -> I.Alias -> Maybe (Env.Type, Env.Exposed Env.Ctor)
aliasToType home name alias =
  aliasToTypeHelp home name <$> I.toPublicAlias alias


aliasToTypeHelp :: ModuleName.Canonical -> Name.Name -> Can.Alias -> (Env.Type, Env.Exposed Env.Ctor)
aliasToTypeHelp home name (Can.Alias vars tipe) =
  (
    Env.Alias (length vars) home vars tipe
  ,
    case tipe of
      Can.TRecord fields Nothing ->
        let
          avars = map (\var -> (var, Can.TVar var)) vars
          alias =
            foldr
              (\(_,t1) t2 -> Can.TLambda t1 t2)
              (Can.TAlias home name avars (Can.Filled tipe))
              (Can.fieldsToList fields)
        in
        Map.singleton name (Map.singleton home (Env.RecordCtor home vars alias))

      _ ->
        Map.empty
  )



-- BINOP


binopToBinop :: ModuleName.Canonical -> Name.Name -> I.Binop -> Map.Map ModuleName.Canonical Env.Binop
binopToBinop home op (I.Binop name annotation associativity precedence) =
  Map.singleton home (Env.Binop op home name annotation associativity precedence)



-- ADD EXPOSED VALUE


addExposedValue
  :: ModuleName.Canonical
  -> Env.Exposed Can.Annotation
  -> Map.Map Name.Name (Env.Type, Env.Exposed Env.Ctor)
  -> Map.Map Name.Name I.Binop
  -> State
  -> Src.Exposed
  -> Result i w State
addExposedValue home vars types binops (State vs ts cs bs qvs qts qcs) exposed =
  case exposed of
    Src.Lower (A.At region name) ->
      case Map.lookup name vars of
        Just info ->
          Result.ok (State (Map.insertWith Map.union name info vs) ts cs bs qvs qts qcs)

        Nothing ->
          Result.throw (Error.ImportExposingNotFound region home name (Map.keys vars))

    Src.Upper (A.At region name) privacy ->
      case privacy of
        Src.Private ->
          case Map.lookup name types of
            Just (tipe, ctors) ->
              case tipe of
                Env.Union _ _ ->
                  let
                    !ts2 = Map.insert name (Map.singleton home tipe) ts
                  in
                  Result.ok (State vs ts2 cs bs qvs qts qcs)

                Env.Alias _ _ _ _ ->
                  let
                    !ts2 = Map.insert name (Map.singleton home tipe) ts
                    !cs2 = addExposed cs ctors
                  in
                  Result.ok (State vs ts2 cs2 bs qvs qts qcs)

            Nothing ->
              case Map.lookup name (toCtors types) of
                Just tipe ->
                  Result.throw $ Error.ImportCtorByName region name tipe

                Nothing ->
                  Result.throw $ Error.ImportExposingNotFound region home name (Map.keys types)

        Src.Public dotDotRegion ->
          case Map.lookup name types of
            Just (tipe, ctors) ->
              case tipe of
                Env.Union _ _ ->
                  let
                    !ts2 = Map.insert name (Map.singleton home tipe) ts
                    !cs2 = addExposed cs ctors
                  in
                  Result.ok (State vs ts2 cs2 bs qvs qts qcs)

                Env.Alias _ _ _ _ ->
                  Result.throw (Error.ImportOpenAlias dotDotRegion name)

            Nothing ->
              Result.throw (Error.ImportExposingNotFound region home name (Map.keys types))

    Src.Operator region op ->
      case Map.lookup op binops of
        Just binop ->
          let
            !bs2 = Map.insert op (binopToBinop home op binop) bs
          in
          Result.ok (State vs ts cs bs2 qvs qts qcs)

        Nothing ->
          Result.throw (Error.ImportExposingNotFound region home op (Map.keys binops))



toCtors :: Map.Map Name.Name (Env.Type, Env.Exposed Env.Ctor) -> Map.Map Name.Name Name.Name
toCtors types =
    Map.foldr addCtors Map.empty types
  where
    addCtors (_, exposedCtors) dict =
      Map.foldrWithKey addCtor dict exposedCtors

    addCtor ctorName homes dict =
      case Map.elems homes of
        [Env.Ctor _ tipeName _ _ _] ->
          Map.insert ctorName tipeName dict

        _ ->
          dict
