{-# OPTIONS_GHC -Wall #-}
module Canonicalize.Sort (definitions) where

import Control.Monad.State
import Control.Applicative ((<$>),(<*>))
import qualified Data.Graph as Graph
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

import AST.Expression.General (Expr'(..), PortImpl(..))
import qualified AST.Expression.Canonical as Canonical
import qualified AST.Pattern as P
import qualified AST.Variable as V
import qualified Reporting.Annotation as A


-- STARTING POINT

definitions :: Canonical.Expr -> Canonical.Expr
definitions expression =
  evalState (reorder expression) Set.empty


free :: String -> State (Set.Set String) ()
free x =
  modify (Set.insert x)


freeIfLocal :: V.Canonical -> State (Set.Set String) ()
freeIfLocal (V.Canonical home name) =
  case home of
    V.Local -> free name
    V.TopLevel _ -> free name
    V.BuiltIn -> return ()
    V.Module _ -> return ()


-- REORDER EXPRESSIONS

reorder :: Canonical.Expr -> State (Set.Set String) Canonical.Expr
reorder (A.A ann expression) =
    A.A ann <$>
    case expression of
      -- Be careful adding and restricting freeVars
      Var var ->
          do  freeIfLocal var
              return expression

      Lambda pattern body ->
          uncurry Lambda <$> bindingReorder (pattern,body)

      Binop op leftExpr rightExpr ->
          do  freeIfLocal op
              Binop op <$> reorder leftExpr <*> reorder rightExpr

      Case expr cases ->
          Case <$> reorder expr <*> mapM bindingReorder cases

      Data name exprs ->
          do  free name
              Data name <$> mapM reorder exprs

      -- Just pipe the reorder though
      Literal _ ->
          return expression

      Range lowExpr highExpr ->
          Range <$> reorder lowExpr <*> reorder highExpr

      ExplicitList es ->
          ExplicitList <$> mapM reorder es

      App func arg ->
          App <$> reorder func <*> reorder arg

      If branches finally ->
          If
            <$> mapM (\(cond,branch) -> (,) <$> reorder cond <*> reorder branch) branches
            <*> reorder finally

      Access record field ->
          Access
            <$> reorder record
            <*> return field

      Update record fields ->
          Update
            <$> reorder record
            <*> mapM (\(field,expr) -> (,) field <$> reorder expr) fields

      Record fields ->
          Record
            <$> mapM (\(field,expr) -> (,) field <$> reorder expr) fields

      GLShader _ _ _ ->
          return expression

      Port impl ->
          Port <$>
            case impl of
              In _ _ ->
                  return impl

              Out name expr tipe ->
                  (\e -> Out name e tipe) <$> reorder expr

              Task name expr tipe ->
                  (\e -> Task name e tipe) <$> reorder expr

      -- Actually do some reordering
      Let defs body ->
          do  body' <- reorder body

              -- Sort defs into strongly connected components.This
              -- allows the programmer to write definitions in whatever
              -- order they please, we can still define things in order
              -- and generalize polymorphic functions when appropriate.
              sccs <- Graph.stronglyConnComp <$> buildDefGraph defs
              let defss = map Graph.flattenSCC sccs

              -- remove let-bound variables from the context
              forM_ defs $ \(Canonical.Definition _ pattern _ _) -> do
                  bound pattern
                  mapM free (ctors pattern)

              let (A.A _ let') =
                    foldr (\ds bod -> A.A ann (Let ds bod)) body' defss

              return let'


ctors :: P.CanonicalPattern -> [String]
ctors (A.A _ pattern) =
    case pattern of
      P.Var _ ->
          []

      P.Alias _ p ->
          ctors p

      P.Record _ ->
          []

      P.Anything ->
          []

      P.Literal _ ->
          []

      P.Data (V.Canonical home name) ps ->
          case home of
            V.Local -> name : rest
            V.TopLevel _ -> name : rest
            V.BuiltIn -> rest
            V.Module _ -> rest
          where
            rest = concatMap ctors ps


bound :: P.CanonicalPattern -> State (Set.Set String) ()
bound pattern =
  let boundVars = P.boundVarSet pattern
  in
      modify (\freeVars -> Set.difference freeVars boundVars)


bindingReorder
    :: (P.CanonicalPattern, Canonical.Expr)
    -> State (Set.Set String) (P.CanonicalPattern, Canonical.Expr)
bindingReorder (pattern, expr) =
  do  expr' <- reorder expr
      bound pattern
      mapM_ free (ctors pattern)
      return (pattern, expr')


-- BUILD DEPENDENCY GRAPH BETWEEN DEFINITIONS

-- This also reorders the all of the sub-expressions in the Def list.
buildDefGraph
    :: [Canonical.Def]
    -> State (Set.Set String) [(Canonical.Def, Int, [Int])]
buildDefGraph defs =
  do  pdefsDeps <- mapM reorderAndGetDependencies defs
      return $ realDeps (addKey pdefsDeps)
  where
    addKey :: [(Canonical.Def, [String])] -> [(Canonical.Def, Int, [String])]
    addKey =
        zipWith (\n (pdef,deps) -> (pdef,n,deps)) [0..]

    variableToKey :: (Canonical.Def, Int, [String]) -> [(String, Int)]
    variableToKey (Canonical.Definition _ pattern _ _, key, _) =
        [ (var, key) | var <- P.boundVarList pattern ]

    variableToKeyMap :: [(Canonical.Def, Int, [String])] -> Map.Map String Int
    variableToKeyMap pdefsDeps =
        Map.fromList (concatMap variableToKey pdefsDeps)

    realDeps :: [(Canonical.Def, Int, [String])] -> [(Canonical.Def, Int, [Int])]
    realDeps pdefsDeps =
        map convert pdefsDeps
      where
        varDict = variableToKeyMap pdefsDeps
        convert (pdef, key, deps) =
            (pdef, key, Maybe.mapMaybe (flip Map.lookup varDict) deps)


reorderAndGetDependencies
    :: Canonical.Def
    -> State (Set.Set String) (Canonical.Def, [String])
reorderAndGetDependencies (Canonical.Definition facts pattern expr mType) =
  do  globalFrees <- get
      -- work in a fresh environment
      put Set.empty
      expr' <- reorder expr
      localFrees <- get
      -- merge with global frees
      modify (Set.union globalFrees)
      return (Canonical.Definition facts pattern expr' mType, Set.toList localFrees)
