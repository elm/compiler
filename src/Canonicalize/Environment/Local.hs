{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Canonicalize.Environment.Local
  ( addVarsTypesOps
  , addPatterns
  )
  where


import Control.Applicative (liftA2)
import Control.Monad (foldM)
import qualified Data.Graph as Graph
import qualified Data.Map.Strict as Map
import qualified Data.Map.Merge.Strict as Map

import qualified AST.Expression.Canonical as Can
import qualified AST.Expression.Valid as Valid
import qualified AST.Type as Type
import qualified Canonicalize.Environment.Dups as Dups
import qualified Canonicalize.Environment.Internals as Env
import qualified Canonicalize.Type as Type
import qualified Data.Bag as Bag
import qualified Data.OneOrMore as OneOrMore
import qualified Elm.Name as N
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Canonicalize as Error
import qualified Reporting.Region as R
import qualified Reporting.Result as Result
import qualified Reporting.Warning as Warning



-- RESULT


type Result a =
  Result.Result () Warning.Warning Error.Error a



-- ADD VARS, TYPES, AND OPS
--
-- We can add names for local definitions, unions, aliases, and operators.
-- These are basically just names. We CANNOT add patterns though. Those must
-- be added by `addPatterns` after some canonicalization has occurred.
--
-- This code checks for (1) duplicate names and (2) cycles in type aliases.


addVarsTypesOps :: Valid.Module -> Env.Env -> Result Env.Env
addVarsTypesOps module_ env =
  addVars module_ =<< addTypes module_ =<< addBinops module_ env


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
      map (ctorToPattern tipe tvars) ctors

    ctorToPattern tipe tvars (name, args) =
      ( name, Env.Pattern home tipe tvars args )
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



-- ADD BINOPS


addBinops :: Valid.Module -> Env.Env -> Result Env.Env
addBinops (Valid.Module _ _ _ _ _ _ _ _ ops _) (Env.Env home vars types patterns binops) =
  let
    toInfo (Valid.Binop (A.A region op) assoc prec name) =
      Dups.info op region () (OneOrMore.one (Env.Binop op home name assoc prec))

    toError op () () =
      Error.DuplicateBinop op
  in
  do  opDict <- Dups.detect toError $ map toInfo ops
      let newBinops = Map.unionWith OneOrMore.more opDict binops
      return $ Env.Env home vars types patterns newBinops



-- ADD VARS


addVars :: Valid.Module -> Env.Env -> Result Env.Env
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


collectLowerVars :: Valid.Module -> Result (Map.Map N.Name ())
collectLowerVars (Valid.Module _ _ _ _ _ decls _ _ _ effects) =
  let
    declToInfo (A.A _ (Valid.Decl (A.A region name) _ _ _)) =
      Dups.info name region () ()

    portToInfo (Valid.Port (A.A region name) _) =
      Dups.info name region () ()

    effectInfo =
      case effects of
        Valid.NoEffects ->
          []

        Valid.Ports ports ->
          map portToInfo ports

        Valid.Manager _ manager ->
          case manager of
            Valid.Cmd (A.A region _) ->
              [ Dups.info "command" region () () ]

            Valid.Sub (A.A region _) ->
              [ Dups.info "subscription" region () () ]

            Valid.Fx (A.A regionCmd _) (A.A regionSub _) ->
              [ Dups.info "command" regionCmd () ()
              , Dups.info "subscription" regionSub () ()
              ]

    toError name () () =
      Error.DuplicateDecl name
  in
  Dups.detect toError $
    effectInfo ++ map declToInfo decls


collectUpperVars :: Valid.Module -> Result (Map.Map N.Name ())
collectUpperVars (Valid.Module _ _ _ _ _ _ unions aliases _ _) =
  let
    unionToInfos (Valid.Union (A.A _ name) _ ctors) =
      map (ctorToInfo name) ctors

    ctorToInfo tipe (A.A region name, _args) =
      Dups.info name region (Error.UnionCtor tipe) ()

    aliasToInfos (Valid.Alias (A.A region name) _ (A.A _ tipe)) =
      case tipe of
        Type.RRecord _ Nothing ->
          [ Dups.info name region Error.RecordCtor () ]

        _ ->
          []
  in
  Dups.detect Error.DuplicateCtor $
    concatMap aliasToInfos aliases ++ concatMap unionToInfos unions



-- ADD TYPES


addTypes :: Valid.Module -> Env.Env -> Result Env.Env
addTypes (Valid.Module _ _ _ _ _ _ unions aliases _ _) env =
  let
    aliasToInfo alias@(Valid.Alias (A.A region name) _ tipe) =
      do  args <- checkAliasFreeVars alias
          return $ Dups.info name region () (Left (Alias region name args tipe))

    unionToInfo union@(Valid.Union (A.A region name) _ _) =
      do  arity <- checkUnionFreeVars union
          return $ Dups.info name region () (Right arity)

    toError name () () =
      Error.DuplicateType name

    infos =
      liftA2 (++)
        (traverse aliasToInfo aliases)
        (traverse unionToInfo unions)
  in
  do  types <- Dups.detect toError =<< infos
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
    , _type :: Type.Raw
    }


addTypeAliases :: Map.Map N.Name Alias -> Env.Env -> Result Env.Env
addTypeAliases aliases env =
  do  let nodes = map toNode (Map.elems aliases)
      let sccs = Graph.stronglyConnComp nodes
      foldM addTypeAlias env sccs


addTypeAlias :: Env.Env -> Graph.SCC Alias -> Result Env.Env
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
      Result.throw region (Error.RecursiveAlias name1 args tipe (map toName others))


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


getEdges :: Type.Raw -> Bag.Bag N.Name
getEdges (A.A _ tipe) =
  case tipe of
    Type.RLambda arg result ->
      Bag.append (getEdges arg) (getEdges result)

    Type.RVar _ ->
      Bag.empty

    Type.RType _ Nothing name args ->
      foldr Bag.append (Bag.one name) (map getEdges args)

    Type.RType _ (Just _) _ args ->
      foldr Bag.append Bag.empty (map getEdges args)

    Type.RRecord fields ext ->
      foldr Bag.append
        (maybe Bag.empty getEdges ext)
        (map (getEdges . snd) fields)

    Type.RUnit ->
      Bag.empty

    Type.RTuple a b cs ->
      foldr Bag.append
        (getEdges a)
        (getEdges b : map getEdges cs)



-- CHECK FREE VARIABLES


checkUnionFreeVars :: Valid.Union -> Result Int
checkUnionFreeVars (Valid.Union (A.A unionRegion name) args ctors) =
  let
    toInfo (A.A region arg) =
      Dups.info arg region () region

    toError badArg () () =
      Error.DuplicateUnionArg name badArg (map A.drop args)

    addCtorFreeVars (_, tipes) freeVars =
      foldr addFreeVars freeVars tipes
  in
  do  boundVars <- Dups.detect toError (map toInfo args)
      let freeVars = foldr addCtorFreeVars Map.empty ctors
      case Map.toList (Map.difference freeVars boundVars) of
        [] ->
          Result.ok (length args)

        unbound ->
          let toLoc (arg, region) = A.A region arg in
          Result.throw unionRegion $
            Error.TypeVarsUnboundInUnion name (map A.drop args) (map toLoc unbound)



checkAliasFreeVars :: Valid.Alias -> Result [N.Name]
checkAliasFreeVars (Valid.Alias (A.A aliasRegion name) args tipe) =
  let
    toInfo (A.A region arg) =
      Dups.info arg region () region

    toError badArg () () =
      Error.DuplicateAliasArg name badArg (map A.drop args)
  in
  do  boundVars <- Dups.detect toError (map toInfo args)
      let freeVars = addFreeVars tipe Map.empty
      let overlap = Map.size (Map.intersection boundVars freeVars)
      if Map.size boundVars == overlap && Map.size freeVars == overlap
        then Result.ok (map A.drop args)
        else
          let toLoc (arg, region) = A.A region arg in
          Result.throw aliasRegion $
            Error.TypeVarsMessedUpInAlias name
              (map A.drop args)
              (map toLoc (Map.toList (Map.difference boundVars freeVars)))
              (map toLoc (Map.toList (Map.difference freeVars boundVars)))


addFreeVars :: Type.Raw -> Map.Map N.Name R.Region -> Map.Map N.Name R.Region
addFreeVars (A.A region tipe) freeVars =
  case tipe of
    Type.RLambda arg result ->
      addFreeVars arg (addFreeVars result freeVars)

    Type.RVar name ->
      Map.insert name region freeVars

    Type.RType _ _ _ args ->
      foldr addFreeVars freeVars args

    Type.RRecord fields ext ->
      foldr addFreeVars (maybe id addFreeVars ext freeVars) (map snd fields)

    Type.RUnit ->
      freeVars

    Type.RTuple a b cs ->
      foldr addFreeVars freeVars (a:b:cs)
