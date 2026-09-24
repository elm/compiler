{-# LANGUAGE QuasiQuotes #-}
module Canonicalize.Environment.Local
  ( add
  )
  where


import Prelude hiding (cycle)
import Control.Monad (foldM)
import qualified Data.List as List
import qualified Data.Map.Strict as Map

import qualified Graph

import qualified AST.Canonical as Can
import qualified AST.Source as Src
import qualified AST.Prim.Name as N
import qualified AST.Prim.TypeName as T
import qualified AST.Prim.TypeVar as T
import qualified Canonicalize.Environment as Env
import qualified Canonicalize.Environment.Dups as Dups
import qualified Canonicalize.Type as Type
import qualified Data.Index as Index
import qualified Elm.ModuleName as ModuleName
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Canonicalize as Error
import qualified Reporting.Result as Result



-- RESULT


type Result i w a =
  Result.Result i w Error.Error a


type Unions = Map.Map T.Name Can.Union
type Aliases = Map.Map T.Name Can.Alias


add :: Src.Module -> Env.Env -> Result i w (Env.Env, Unions, Aliases)
add module_ env =
  addCtors module_ =<< addVars module_ =<< addTypes module_ env



-- ADD VARS


addVars :: Src.Module -> Env.Env -> Result i w Env.Env
addVars module_ (Env.Env home vs ts cs bs qvs qts qcs) =
  do  topLevelVars <- collectVars module_
      let vs2 = Map.union topLevelVars vs
      -- Use union to overwrite foreign stuff.
      Result.ok $ Env.Env home vs2 ts cs bs qvs qts qcs


collectVars :: Src.Module -> Result i w (Map.Map N.Name Env.Var)
collectVars (Src.Module _ _ _ _ values _ _ _ effects) =
  let
    addDecl dict (A.At _ (Src.Value (A.At region name) _ _ _)) =
      Dups.insert name region (Env.TopLevel region) dict
  in
  Dups.detect Error.DuplicateDecl $
    List.foldl' addDecl (toEffectDups effects) values


toEffectDups :: Src.Effects -> Dups.Dict N.Name Env.Var
toEffectDups effects =
  case effects of
    Src.NoEffects ->
      Dups.none

    Src.Ports ports ->
      let
        addPort dict (Src.Port (A.At region name) _) =
          Dups.insert name region (Env.TopLevel region) dict
      in
      List.foldl' addPort Dups.none ports

    Src.Manager _ manager ->
      case manager of
        Src.Cmd (A.At region _) ->
          Dups.one [N.ascii|command|] region (Env.TopLevel region)

        Src.Sub (A.At region _) ->
          Dups.one [N.ascii|subscription|] region (Env.TopLevel region)

        Src.Fx (A.At regionCmd _) (A.At regionSub _) ->
          Dups.union
            (Dups.one [N.ascii|command|] regionCmd (Env.TopLevel regionCmd))
            (Dups.one [N.ascii|subscription|] regionSub (Env.TopLevel regionSub))



-- ADD TYPES


addTypes :: Src.Module -> Env.Env -> Result i w Env.Env
addTypes (Src.Module _ _ _ _ _ unions aliases _ _) (Env.Env home vs ts cs bs qvs qts qcs) =
  let
    addAliasDups dups (A.At _ (Src.Alias (A.At region name) _ _)) = Dups.insert name region () dups
    addUnionDups dups (A.At _ (Src.Union (A.At region name) _ _)) = Dups.insert name region () dups
    typeNameDups =
      List.foldl' addUnionDups (List.foldl' addAliasDups Dups.none aliases) unions
  in
  do  _ <- Dups.detect Error.DuplicateType typeNameDups
      ts1 <- foldM (addUnion home) ts unions
      addAliases aliases (Env.Env home vs ts1 cs bs qvs qts qcs)


addUnion :: ModuleName.Canonical -> Env.Exposed T.Name Env.Type -> A.Located Src.Union -> Result i w (Env.Exposed T.Name Env.Type)
addUnion home types union@(A.At _ (Src.Union (A.At _ name) _ _)) =
  do  arity <- checkUnionFreeVars union
      let one = Env.Specific home (Env.Union arity home)
      Result.ok $ Map.insert name one types



-- ADD TYPE ALIASES


addAliases :: [A.Located Src.Alias] -> Env.Env -> Result i w Env.Env
addAliases aliases env =
  let
    nodes = map toNode aliases
    sccs = Graph.toSCC nodes
  in
  foldM addAlias env sccs


addAlias :: Env.Env -> Graph.SCC T.Name (A.Located Src.Alias) -> Result i w Env.Env
addAlias env@(Env.Env home vs ts cs bs qvs qts qcs) scc =
  case scc of
    Graph.Acyclic (Graph.Node _ alias@(A.At _ (Src.Alias (A.At _ name) _ tipe)) _) ->
      do  args <- checkAliasFreeVars alias
          ctype <- Type.canonicalize env tipe
          let one = Env.Specific home (Env.Alias (length args) home args ctype)
          let ts1 = Map.insert name one ts
          Result.ok $ Env.Env home vs ts1 cs bs qvs qts qcs

    Graph.Cyclic aliases ->
      Graph.withMinimalCycle selector aliases Graph._key $ \alias@(A.At _ (Src.Alias (A.At region name1) _ tipe)) cycle ->
        do  args <- checkAliasFreeVars alias
            Result.throw (Error.RecursiveAlias region name1 args tipe cycle)
  where
    selector = Graph.RootSelector $ \(Graph.Node k v _) _ -> (k, v)



-- DETECT TYPE ALIAS CYCLES


toNode :: A.Located Src.Alias -> Graph.Node T.Name (A.Located Src.Alias)
toNode alias@(A.At _ (Src.Alias (A.At _ name) _ tipe)) =
  Graph.Node name alias (getEdges [] tipe)


getEdges :: [T.Name] -> Src.Type -> [T.Name]
getEdges edges (A.At _ tipe) =
  case tipe of
    Src.TLambda a b        -> getEdges (getEdges edges a) b
    Src.TVar _             -> edges
    Src.TType     _   n xs -> List.foldl' getEdges (n:edges) xs
    Src.TTypeQual _ _ _ xs -> List.foldl' getEdges edges xs
    Src.TRecord fs _       -> List.foldl' (\es (_,t) -> getEdges es t) edges fs
    Src.TUnit              -> edges
    Src.TTuple a b cs      -> List.foldl' getEdges (getEdges (getEdges edges a) b) cs



-- CHECK FREE VARIABLES


checkUnionFreeVars :: A.Located Src.Union -> Result i w Int
checkUnionFreeVars (A.At unionRegion (Src.Union (A.At _ name) args ctors)) =
  let
    addArg (A.At region arg) dict =
      Dups.insert arg region region dict

    addCtorFreeVars (_, tipes) freeVars =
      List.foldl' addFreeVars freeVars tipes
  in
  do  boundVars <- Dups.detect (Error.DuplicateUnionArg name) (foldr addArg Dups.none args)
      let freeVars = foldr addCtorFreeVars Map.empty ctors
      case Map.toList (Map.difference freeVars boundVars) of
        [] ->
          Result.ok (length args)

        unbound:unbounds ->
          Result.throw $
            Error.TypeVarsUnboundInUnion unionRegion name (map A.toValue args) unbound unbounds


checkAliasFreeVars :: A.Located Src.Alias -> Result i w [T.Var]
checkAliasFreeVars (A.At aliasRegion (Src.Alias (A.At _ name) args tipe)) =
  let
    addArg (A.At region arg) dict =
      Dups.insert arg region region dict
  in
  do  boundVars <- Dups.detect (Error.DuplicateAliasArg name) (foldr addArg Dups.none args)
      let freeVars = addFreeVars Map.empty tipe
      let overlap = Map.size (Map.intersection boundVars freeVars)
      if Map.size boundVars == overlap && Map.size freeVars == overlap
        then Result.ok (map A.toValue args)
        else
          Result.throw $
            Error.TypeVarsMessedUpInAlias aliasRegion name
              (map A.toValue args)
              (Map.toList (Map.difference boundVars freeVars))
              (Map.toList (Map.difference freeVars boundVars))


addFreeVars :: Map.Map T.Var A.Region -> Src.Type -> Map.Map T.Var A.Region
addFreeVars freeVars (A.At region tipe) =
  case tipe of
    Src.TLambda a b        -> addFreeVars (addFreeVars freeVars a) b
    Src.TVar x             -> Map.insert x region freeVars
    Src.TType     _   _ xs -> List.foldl' addFreeVars freeVars xs
    Src.TTypeQual _ _ _ xs -> List.foldl' addFreeVars freeVars xs
    Src.TRecord fs e       -> List.foldl' (\fvs (_,t) -> addFreeVars fvs t) (addExt e freeVars) fs
    Src.TUnit              -> freeVars
    Src.TTuple a b cs      -> List.foldl' addFreeVars (addFreeVars (addFreeVars freeVars a) b) cs
  where
    addExt ext fvs =
      case ext of
        Nothing         -> fvs
        Just (A.At r x) -> Map.insert x r fvs



-- ADD CTORS


addCtors :: Src.Module -> Env.Env -> Result i w (Env.Env, Unions, Aliases)
addCtors (Src.Module _ _ _ _ _ unions aliases _ _) env@(Env.Env home vs ts cs bs qvs qts qcs) =
  do  unionInfo <- traverse (canonicalizeUnion env) unions
      aliasInfo <- traverse (canonicalizeAlias env) aliases

      ctors <-
        Dups.detect Error.DuplicateCtor $
          Dups.union
            (Dups.unions (map snd unionInfo))
            (Dups.unions (map snd aliasInfo))

      let cs2 = Map.union ctors cs

      Result.ok
        ( Env.Env home vs ts cs2 bs qvs qts qcs
        , Map.fromList (map fst unionInfo)
        , Map.fromList (map fst aliasInfo)
        )


type CtorDups = Dups.Dict N.Name (Env.Info Env.Ctor)



-- CANONICALIZE ALIAS


canonicalizeAlias :: Env.Env -> A.Located Src.Alias -> Result i w ( (T.Name, Can.Alias), CtorDups )
canonicalizeAlias env@(Env.Env home _ _ _ _ _ _ _) (A.At _ (Src.Alias (A.At region name) args tipe)) =
  do  let vars = map A.toValue args
      ctipe <- Type.canonicalize env tipe
      Result.ok
        ( (name, Can.Alias vars ctipe)
        ,
          case ctipe of
            Can.TRecord fields Nothing ->
              Dups.one (T.nameToName name) region (Env.Specific home (toRecordCtor home name vars fields))

            _ ->
              Dups.none
        )

toRecordCtor :: ModuleName.Canonical -> T.Name -> [T.Var] -> Map.Map N.Name Can.FieldType -> Env.Ctor
toRecordCtor home name vars fields =
  let
    avars = map (\var -> (var, Can.TVar var)) vars
    alias =
      foldr
        (\(_,t1) t2 -> Can.TLambda t1 t2)
        (Can.TAlias home name avars (Can.Filled (Can.TRecord fields Nothing)))
        (Can.fieldsToList fields)
  in
  Env.RecordCtor home vars alias



-- CANONICALIZE UNION


canonicalizeUnion :: Env.Env -> A.Located Src.Union -> Result i w ( (T.Name, Can.Union), CtorDups )
canonicalizeUnion env@(Env.Env home _ _ _ _ _ _ _) (A.At _ (Src.Union (A.At _ name) avars ctors)) =
  do  cctors <- Index.indexedTraverse (canonicalizeCtor env) ctors
      let vars = map A.toValue avars
      let alts = map A.toValue cctors
      let union = Can.Union vars alts (length alts) (toOpts ctors)
      Result.ok
        ( (name, union)
        , Dups.unions $ map (toCtor home name union) cctors
        )


canonicalizeCtor :: Env.Env -> Index.ZeroBased -> (A.Located N.Name, [Src.Type]) -> Result i w (A.Located Can.Ctor)
canonicalizeCtor env index (A.At region ctor, tipes) =
  do  ctipes <- traverse (Type.canonicalize env) tipes
      Result.ok $ A.At region $
        Can.Ctor ctor index (length ctipes) ctipes


toOpts :: [(A.Located N.Name, [Src.Type])] -> Can.CtorOpts
toOpts ctors =
  case ctors of
    [ (_,[_]) ] ->
      Can.Unbox

    _ ->
      if all (null . snd) ctors then Can.Enum else Can.Normal


toCtor :: ModuleName.Canonical -> T.Name -> Can.Union -> A.Located Can.Ctor -> CtorDups
toCtor home typeName union (A.At region (Can.Ctor name index _ args)) =
  Dups.one name region $ Env.Specific home $
    Env.Ctor home typeName union index args
