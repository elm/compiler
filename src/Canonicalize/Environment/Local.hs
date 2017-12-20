{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Canonicalize.Environment.Local
  ( addVarsAndTypes
  , addPatterns
  )
  where


import Control.Applicative (liftA2)
import Control.Monad (foldM)
import qualified Data.Graph as Graph
import qualified Data.Map.Strict as Map
import qualified Data.Map.Merge.Strict as Map

import qualified AST.Canonical as Can
import qualified AST.Source as Src
import qualified AST.Valid as Valid
import qualified Canonicalize.Environment as Env
import qualified Canonicalize.Environment.Dups as Dups
import qualified Canonicalize.Type as Type
import qualified Data.Bag as Bag
import qualified Data.Index as Index
import qualified Elm.Name as N
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Canonicalize as Error
import qualified Reporting.Region as R
import qualified Reporting.Result as Result
import qualified Reporting.Warning as Warning



-- RESULT


type Result i a =
  Result.Result i [Warning.Warning] Error.Error a



-- ADD VARS, TYPES, AND OPS
--
-- We can add names for local definitions, unions, aliases, and operators.
-- These are basically just names. We CANNOT add patterns though. Those must
-- be added by `addPatterns` after some canonicalization has occurred.
--
-- This code checks for (1) duplicate names and (2) cycles in type aliases.


addVarsAndTypes :: Valid.Module -> Env.Env -> Result i Env.Env
addVarsAndTypes module_ env =
  addVars module_ =<< addTypes module_ env


merge
  :: (N.Name -> a -> homes)
  -> (N.Name -> a -> homes -> homes)
  -> Map.Map N.Name a
  -> Map.Map N.Name homes
  -> Map.Map N.Name homes
merge addLeft addBoth locals foreigns =
  let addRight _ homes = homes in
  Map.merge
    (Map.mapMissing addLeft)
    (Map.mapMissing addRight)
    (Map.zipWithMatched addBoth)
    locals
    foreigns



-- ADD PATTERNS
--
-- Local patterns are added to the environment later such that we can have
-- canonical union types. This way we can avoid looking up the types during
-- type constraint generation.
--
-- This function does not return a Result because Dups.detect is already called
-- in collectUpperVars. This will detect all local pattern clashes. We skip it
-- here to avoid a second version of the same error.


addPatterns :: Map.Map N.Name Can.Union -> Env.Env -> Env.Env
addPatterns unions (Env.Env home vars types patterns binops) =
  let
    localPatterns =
      Map.fromList $ concatMap toPatterns (Map.toList unions)

    toPatterns (tipe, Can.Union tvars ctors) =
      Index.indexedMap (ctorToPattern tipe tvars (Can.ctorsToAlts ctors)) ctors

    ctorToPattern tipe tvars alts index (name, args) =
      ( name, Env.Pattern home tipe tvars alts index args )
  in
  let
    newPatterns =
      merge addLeft addBoth localPatterns patterns

    addLeft _ localPattern =
      Env.Homes (Bag.one localPattern) Map.empty

    addBoth _ localPattern (Env.Homes _ qualified) =
      Env.Homes (Bag.one localPattern) qualified
  in
  Env.Env home vars types newPatterns binops



-- ADD VARS


addVars :: Valid.Module -> Env.Env -> Result i Env.Env
addVars module_ (Env.Env home vars types patterns binops) =
  do  upperDict <- collectUpperVars module_
      lowerDict <- collectLowerVars module_
      let varDict = Map.union upperDict lowerDict
      let newVars = addVarsHelp varDict vars
      return $ Env.Env home newVars types patterns binops


addVarsHelp :: Map.Map N.Name () -> Map.Map N.Name Env.VarHomes -> Map.Map N.Name Env.VarHomes
addVarsHelp localVars vars =
  let
    varHomes =
      Env.VarHomes Env.TopLevel Map.empty

    addLeft _ _ =
      varHomes

    addBoth _ _ (Env.VarHomes _ qualified) =
      Env.VarHomes Env.TopLevel qualified
  in
  merge addLeft addBoth localVars vars


collectLowerVars :: Valid.Module -> Result i (Map.Map N.Name ())
collectLowerVars (Valid.Module _ _ _ _ _ decls _ _ _ effects) =
  let
    addDecl (A.At _ (Valid.Decl (A.At region name) _ _ _)) dict =
      Dups.insert name region () dict

    addPort (Valid.Port (A.At region name) _) dict =
      Dups.insert name region () dict

    effectDict =
      case effects of
        Valid.NoEffects ->
          Dups.none

        Valid.Ports ports ->
          foldr addPort Dups.none ports

        Valid.Manager _ manager ->
          case manager of
            Valid.Cmd (A.At region _) ->
              Dups.one "command" region ()

            Valid.Sub (A.At region _) ->
              Dups.one "subscription" region ()

            Valid.Fx (A.At regionCmd _) (A.At regionSub _) ->
              Dups.union
                (Dups.one "command" regionCmd ())
                (Dups.one "subscription" regionSub ())
  in
  Dups.detect Error.DuplicateDecl $ foldr addDecl effectDict decls


collectUpperVars :: Valid.Module -> Result i (Map.Map N.Name ())
collectUpperVars (Valid.Module _ _ _ _ _ _ unions aliases _ _) =
  let
    addUnion (Valid.Union _ _ ctors) dict =
      foldr addCtor dict ctors

    addCtor (A.At region name, _args) dict =
      Dups.insert name region () dict

    addAlias (Valid.Alias (A.At region name) _ (A.At _ tipe)) dict =
      case tipe of
        Src.TRecord _ Nothing ->
          Dups.insert name region () dict

        _ ->
          dict
  in
  Dups.detect Error.DuplicateCtor $
    foldr addAlias (foldr addUnion Dups.none unions) aliases



-- ADD TYPES


addTypes :: Valid.Module -> Env.Env -> Result i Env.Env
addTypes (Valid.Module _ _ _ _ _ _ unions aliases _ _) env =
  let
    aliasToDict alias@(Valid.Alias (A.At region name) _ tipe) =
      do  args <- checkAliasFreeVars alias
          return $ Dups.one name region (Left (Alias region name args tipe))

    unionToDict union@(Valid.Union (A.At region name) _ _) =
      do  arity <- checkUnionFreeVars union
          return $ Dups.one name region (Right arity)

    combineDicts dicts1 dicts2 =
      Dups.union (Dups.unions dicts1) (Dups.unions dicts2)
  in
  do  dict <-
        liftA2 combineDicts
          (traverse aliasToDict aliases)
          (traverse unionToDict unions)
      types <- Dups.detect Error.DuplicateType dict
      let (aliasDict, unionDict) = Map.mapEither id types
      addTypeAliases aliasDict (addUnionTypes unionDict env)


addUnionTypes :: Map.Map N.Name Int -> Env.Env -> Env.Env
addUnionTypes unions (Env.Env home vars types patterns binops) =
  let
    addLeft _ arity =
      Env.Homes (Bag.one (Env.Union arity home)) Map.empty

    addBoth _ arity (Env.Homes _ qualified) =
      Env.Homes (Bag.one (Env.Union arity home)) qualified

    newTypes =
      merge addLeft addBoth unions types
  in
  Env.Env home vars newTypes patterns binops



-- ADD TYPE ALIASES


data Alias =
  Alias
    { _region :: R.Region
    , _name :: N.Name
    , _args :: [N.Name]
    , _type :: Src.Type
    }


addTypeAliases :: Map.Map N.Name Alias -> Env.Env -> Result i Env.Env
addTypeAliases aliases env =
  do  let nodes = map toNode (Map.elems aliases)
      let sccs = Graph.stronglyConnComp nodes
      foldM addTypeAlias env sccs


addTypeAlias :: Env.Env -> Graph.SCC Alias -> Result i Env.Env
addTypeAlias env@(Env.Env home vars types patterns binops) scc =
  case scc of
    Graph.AcyclicSCC (Alias _ name args tipe) ->
      do  ctype <- Type.canonicalize env tipe
          let info = Env.Alias (length args) home args ctype
          let newTypes = Map.alter (addAliasInfo info) name types
          return $ Env.Env home vars newTypes patterns binops

    Graph.CyclicSCC [] ->
      Result.ok env

    Graph.CyclicSCC (Alias region name1 args tipe : others) ->
      let toName (Alias _ name _ _) = name in
      Result.throw (Error.RecursiveAlias region name1 args tipe (map toName others))


addAliasInfo :: a -> Maybe (Env.Homes a) -> Maybe (Env.Homes a)
addAliasInfo info maybeHomes =
  Just $
    case maybeHomes of
      Nothing ->
        Env.Homes (Bag.one info) Map.empty

      Just (Env.Homes _ qualified) ->
        Env.Homes (Bag.one info) qualified



-- DETECT TYPE ALIAS CYCLES


toNode :: Alias -> ( Alias, N.Name, [N.Name] )
toNode alias@(Alias _ name _ tipe) =
  ( alias, name, Bag.toList (getEdges tipe) )


getEdges :: Src.Type -> Bag.Bag N.Name
getEdges (A.At _ tipe) =
  case tipe of
    Src.TLambda arg result ->
      Bag.append (getEdges arg) (getEdges result)

    Src.TVar _ ->
      Bag.empty

    Src.TType _ Nothing name args ->
      foldr Bag.append (Bag.one name) (map getEdges args)

    Src.TType _ (Just _) _ args ->
      foldr Bag.append Bag.empty (map getEdges args)

    Src.TRecord fields ext ->
      foldr Bag.append
        (maybe Bag.empty getEdges ext)
        (map (getEdges . snd) fields)

    Src.TUnit ->
      Bag.empty

    Src.TTuple a b cs ->
      foldr Bag.append
        (getEdges a)
        (getEdges b : map getEdges cs)



-- CHECK FREE VARIABLES


checkUnionFreeVars :: Valid.Union -> Result i Int
checkUnionFreeVars (Valid.Union (A.At unionRegion name) args ctors) =
  let
    addUnion (A.At region arg) dict =
      Dups.insert arg region region dict

    addCtorFreeVars (_, tipes) freeVars =
      foldr addFreeVars freeVars tipes
  in
  do  boundVars <- Dups.detect (Error.DuplicateUnionArg name) (foldr addUnion Dups.none args)
      let freeVars = foldr addCtorFreeVars Map.empty ctors
      case Map.toList (Map.difference freeVars boundVars) of
        [] ->
          Result.ok (length args)

        unbound ->
          let toLoc (arg, region) = A.At region arg in
          Result.throw $
            Error.TypeVarsUnboundInUnion unionRegion name (map A.toValue args) (map toLoc unbound)



checkAliasFreeVars :: Valid.Alias -> Result i [N.Name]
checkAliasFreeVars (Valid.Alias (A.At aliasRegion name) args tipe) =
  let
    addAlias (A.At region arg) dict =
      Dups.insert arg region region dict
  in
  do  boundVars <- Dups.detect (Error.DuplicateAliasArg name) (foldr addAlias Dups.none args)
      let freeVars = addFreeVars tipe Map.empty
      let overlap = Map.size (Map.intersection boundVars freeVars)
      if Map.size boundVars == overlap && Map.size freeVars == overlap
        then Result.ok (map A.toValue args)
        else
          let toLoc (arg, region) = A.At region arg in
          Result.throw $
            Error.TypeVarsMessedUpInAlias aliasRegion name
              (map A.toValue args)
              (map toLoc (Map.toList (Map.difference boundVars freeVars)))
              (map toLoc (Map.toList (Map.difference freeVars boundVars)))


addFreeVars :: Src.Type -> Map.Map N.Name R.Region -> Map.Map N.Name R.Region
addFreeVars (A.At region tipe) freeVars =
  case tipe of
    Src.TLambda arg result ->
      addFreeVars arg (addFreeVars result freeVars)

    Src.TVar name ->
      Map.insert name region freeVars

    Src.TType _ _ _ args ->
      foldr addFreeVars freeVars args

    Src.TRecord fields Nothing ->
      foldr addFreeVars freeVars (map snd fields)

    Src.TRecord fields (Just ext) ->
      foldr addFreeVars (addFreeVars ext freeVars) (map snd fields)

    Src.TUnit ->
      freeVars

    Src.TTuple a b cs ->
      foldr addFreeVars freeVars (a:b:cs)
