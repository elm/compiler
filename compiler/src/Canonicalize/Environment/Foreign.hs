{-# LANGUAGE BangPatterns, OverloadedStrings, TemplateHaskell #-}
module Canonicalize.Environment.Foreign
  ( createInitialEnv
  )
  where


import Control.Monad (foldM)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Map.Utils as Map

import qualified Crash

import qualified AST.Canonical as Can
import qualified AST.Source as Src
import qualified AST.Prim.Module as Module
import qualified AST.Prim.Name as N
import qualified AST.Prim.Operator as Op
import qualified AST.Prim.TypeName as T
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


createInitialEnv :: ModuleName.Canonical -> Map.Map Module.Name I.Interface -> [Src.Import] -> Result i w Env.Env
createInitialEnv home ifaces imports =
  do  (State vs ts cs bs qvs qts qcs) <- foldM (addImport ifaces) emptyState (toSafeImports home imports)
      Result.ok (Env.Env home (Map.map infoToVar vs) ts cs bs qvs qts qcs)


infoToVar :: Env.Info Can.Annotation -> Env.Var
infoToVar info =
  case info of
    Env.Specific home tipe -> Env.Foreign home tipe
    Env.Ambiguous h hs     -> Env.Foreigns h hs



-- STATE


data State =
  State
    { _vars :: Env.Exposed N.Name Can.Annotation
    , _types :: Env.Exposed T.Name Env.Type
    , _ctors :: Env.Exposed N.Name Env.Ctor
    , _binops :: Env.Exposed Op.Name Env.Binop
    , _q_vars :: Env.Qualified N.Name Can.Annotation
    , _q_types :: Env.Qualified T.Name Env.Type
    , _q_ctors :: Env.Qualified N.Name Env.Ctor
    }


emptyState :: State
emptyState =
  State Map.empty emptyTypes Map.empty Map.empty Map.empty Map.empty Map.empty


emptyTypes :: Env.Exposed T.Name Env.Type
emptyTypes =
  Map.singleton T.list (Env.Specific ModuleName.list (Env.Union 1 ModuleName.list))



-- TO SAFE IMPORTS


toSafeImports :: ModuleName.Canonical -> [Src.Import] -> [Src.Import]
toSafeImports (ModuleName.Canonical pkg _) imports =
  if Pkg.isKernel pkg
  then filter isNormal imports
  else imports


isNormal :: Src.Import -> Bool
isNormal (Src.Import (A.At _ name) maybeAlias _) =
  if Module.isKernel name
  then
    case maybeAlias of
      Nothing -> False
      Just _  -> $(Crash.crash 'isNormal) "kernel imports cannot use `as`"
  else
    True



-- ADD IMPORTS


addImport :: Map.Map Module.Name I.Interface -> State -> Src.Import -> Result i w State
addImport ifaces (State vs ts cs bs qvs qts qcs) (Src.Import (A.At _ name) maybeAlias exposing) =
  let
    (I.Interface pkg defs unions aliases binops) = $(Map.require 'addImport) name ifaces Module.toChars
    !prefix = case maybeAlias of { Just p -> p ; Nothing -> Module.toPrefix name }
    !home = ModuleName.Canonical pkg name

    !rawTypeInfo =
      Map.union
        (Map.mapMaybeWithKey (unionToType home) unions)
        (Map.mapMaybeWithKey (aliasToType home) aliases)

    !vars = Map.map (Env.Specific home) defs
    !types = Map.map (Env.Specific home . fst) rawTypeInfo
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


addExposed :: (Ord k) => Env.Exposed k a -> Env.Exposed k a -> Env.Exposed k a
addExposed =
  Map.unionWith Env.mergeInfo


addQualified :: (Ord k) => Module.Prefix -> Env.Exposed k a -> Env.Qualified k a -> Env.Qualified k a
addQualified prefix exposed qualified =
  Map.insertWith addExposed prefix exposed qualified



-- UNION


unionToType :: ModuleName.Canonical -> T.Name -> I.Union -> Maybe (Env.Type, Env.Exposed N.Name Env.Ctor)
unionToType home name union =
  unionToTypeHelp home name <$> I.toPublicUnion union


unionToTypeHelp :: ModuleName.Canonical -> T.Name -> Can.Union -> (Env.Type, Env.Exposed N.Name Env.Ctor)
unionToTypeHelp home name union@(Can.Union vars ctors _ _) =
  let
    addCtor dict (Can.Ctor ctor index _ args) =
      Map.insert ctor (Env.Specific home (Env.Ctor home name union index args)) dict
  in
  ( Env.Union (length vars) home
  , List.foldl' addCtor Map.empty ctors
  )



-- ALIAS


aliasToType :: ModuleName.Canonical -> T.Name -> I.Alias -> Maybe (Env.Type, Env.Exposed N.Name Env.Ctor)
aliasToType home name alias =
  aliasToTypeHelp home name <$> I.toPublicAlias alias


aliasToTypeHelp :: ModuleName.Canonical -> T.Name -> Can.Alias -> (Env.Type, Env.Exposed N.Name Env.Ctor)
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
        Map.singleton (T.nameToName name) (Env.Specific home (Env.RecordCtor home vars alias))

      _ ->
        Map.empty
  )



-- BINOP


binopToBinop :: ModuleName.Canonical -> Op.Name -> I.Binop -> Env.Info Env.Binop
binopToBinop home op (I.Binop name annotation associativity precedence) =
  Env.Specific home (Env.Binop op home name annotation associativity precedence)



-- ADD EXPOSED VALUE


addExposedValue
  :: ModuleName.Canonical
  -> Env.Exposed N.Name Can.Annotation
  -> Map.Map T.Name (Env.Type, Env.Exposed N.Name Env.Ctor)
  -> Map.Map Op.Name I.Binop
  -> State
  -> Src.Exposed
  -> Result i w State
addExposedValue home vars types binops (State vs ts cs bs qvs qts qcs) exposed =
  case exposed of
    Src.Lower (A.At region name) ->
      case Map.lookup name vars of
        Just info ->
          Result.ok (State (Map.insertWith Env.mergeInfo name info vs) ts cs bs qvs qts qcs)

        Nothing ->
          Result.throw (Error.ImportExposedValueNotFound region home name (Map.keys vars))

    Src.Upper (A.At region name) privacy ->
      case privacy of
        Src.Private ->
          case Map.lookup name types of
            Just (tipe, ctors) ->
              case tipe of
                Env.Union _ _ ->
                  let
                    !ts2 = Map.insert name (Env.Specific home tipe) ts
                  in
                  Result.ok (State vs ts2 cs bs qvs qts qcs)

                Env.Alias _ _ _ _ ->
                  let
                    !ts2 = Map.insert name (Env.Specific home tipe) ts
                    !cs2 = addExposed cs ctors
                  in
                  Result.ok (State vs ts2 cs2 bs qvs qts qcs)

            Nothing ->
              case checkForCtorMistake (T.nameToName name) types of
                tipe:_ ->
                  Result.throw $ Error.ImportCtorByName region (T.nameToName name) tipe

                [] ->
                  Result.throw $ Error.ImportExposedTypeNotFound region home name (Map.keys types)

        Src.Public dotDotRegion ->
          case Map.lookup name types of
            Just (tipe, ctors) ->
              case tipe of
                Env.Union _ _ ->
                  let
                    !ts2 = Map.insert name (Env.Specific home tipe) ts
                    !cs2 = addExposed cs ctors
                  in
                  Result.ok (State vs ts2 cs2 bs qvs qts qcs)

                Env.Alias _ _ _ _ ->
                  Result.throw (Error.ImportOpenAlias dotDotRegion name)

            Nothing ->
              Result.throw (Error.ImportExposedTypeNotFound region home name (Map.keys types))

    Src.Operator region op ->
      case Map.lookup op binops of
        Just binop ->
          let
            !bs2 = Map.insert op (binopToBinop home op binop) bs
          in
          Result.ok (State vs ts cs bs2 qvs qts qcs)

        Nothing ->
          Result.throw (Error.ImportExposedBinopNotFound region home op (Map.keys binops))


checkForCtorMistake :: N.Name -> Map.Map T.Name (Env.Type, Env.Exposed N.Name Env.Ctor) -> [T.Name]
checkForCtorMistake givenName types =
    Map.foldr addMatches [] types
  where
    addMatches (_, exposedCtors) matches =
      Map.foldrWithKey addMatch matches exposedCtors

    addMatch ctorName info matches =
      if ctorName /= givenName
      then matches
      else
        case info of
          Env.Specific _ (Env.Ctor _ tipeName _ _ _) -> tipeName : matches
          Env.Specific _ (Env.RecordCtor _ _ _)      -> matches
          Env.Ambiguous _ _                          -> matches
