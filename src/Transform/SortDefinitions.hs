{-# OPTIONS_GHC -Wall #-}
module Transform.SortDefinitions (sortDefs) where

import Control.Monad.State
import Control.Applicative ((<$>),(<*>))
import qualified Data.Graph as Graph
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

import AST.Annotation
import AST.Expression.General (Expr'(..))
import qualified AST.Expression.Canonical as Canonical
import qualified AST.Pattern as P
import qualified AST.Variable as V

ctors :: P.CanonicalPattern -> [String]
ctors pattern =
    case pattern of
      P.Var _ -> []
      P.Alias _ p -> ctors p
      P.Record _ -> []
      P.Anything -> []
      P.Literal _ -> []
      P.Data (V.Canonical home name) ps ->
          case home of
            V.Local -> name : rest
            V.BuiltIn -> rest
            V.Module _ -> rest
          where
            rest = concatMap ctors ps

free :: String -> State (Set.Set String) ()
free x = modify (Set.insert x)

freeIfLocal :: V.Canonical -> State (Set.Set String) ()
freeIfLocal (V.Canonical home name) =
    do case home of
         V.Local -> free name
         V.BuiltIn -> return ()
         V.Module _ -> return ()

bound :: Set.Set String -> State (Set.Set String) ()
bound boundVars = modify (\freeVars -> Set.difference freeVars boundVars)

sortDefs :: Canonical.Expr -> Canonical.Expr
sortDefs expr = evalState (reorder expr) Set.empty

reorder :: Canonical.Expr -> State (Set.Set String) Canonical.Expr
reorder (A ann expr) =
    A ann <$>
    case expr of
      -- Be careful adding and restricting freeVars
      Var var -> freeIfLocal var >> return expr

      Lambda p e ->
          uncurry Lambda <$> bindingReorder (p,e)

      Binop op e1 e2 ->
          do freeIfLocal op
             Binop op <$> reorder e1 <*> reorder e2

      Case e cases ->
          Case <$> reorder e <*> mapM bindingReorder cases

      Data name es ->
          do free name
             Data name <$> mapM reorder es

      -- Just pipe the reorder though
      Literal _ -> return expr

      Range e1 e2 ->
          Range <$> reorder e1 <*> reorder e2

      ExplicitList es ->
          ExplicitList <$> mapM reorder es

      App e1 e2 ->
          App <$> reorder e1 <*> reorder e2

      MultiIf branches ->
          MultiIf <$> mapM (\(e1,e2) -> (,) <$> reorder e1 <*> reorder e2) branches

      Access e lbl ->
          Access <$> reorder e <*> return lbl

      Remove e lbl ->
          Remove <$> reorder e <*> return lbl

      Insert e lbl v ->
          Insert <$> reorder e <*> return lbl <*> reorder v

      Modify e fields ->
          Modify <$> reorder e <*> mapM (\(k,v) -> (,) k <$> reorder v) fields

      Record fields ->
          Record <$> mapM (\(k,v) -> (,) k <$> reorder v) fields

      GLShader _ _ _ -> return expr

      PortOut name st signal -> PortOut name st <$> reorder signal

      PortIn name st -> return $ PortIn name st

      -- Actually do some reordering
      Let defs body ->
          do body' <- reorder body

             -- Sort defs into strongly connected components.This
             -- allows the programmer to write definitions in whatever
             -- order they please, we can still define things in order
             -- and generalize polymorphic functions when appropriate.
             sccs <- Graph.stronglyConnComp <$> buildDefDict defs
             let defss = map Graph.flattenSCC sccs
             
             -- remove let-bound variables from the context
             forM_ defs $ \(Canonical.Definition pattern _ _) -> do
                bound (P.boundVars pattern)
                mapM free (ctors pattern)

             let A _ let' = foldr (\ds bod -> A ann (Let ds bod)) body' defss

             return let'

bindingReorder :: (P.CanonicalPattern, Canonical.Expr)
               -> State (Set.Set String) (P.CanonicalPattern, Canonical.Expr)
bindingReorder (pattern,expr) =
    do expr' <- reorder expr
       bound (P.boundVars pattern)
       mapM_ free (ctors pattern)
       return (pattern, expr')


reorderAndGetDependencies :: Canonical.Def -> State (Set.Set String) (Canonical.Def, [String])
reorderAndGetDependencies (Canonical.Definition pattern expr mType) =
    do globalFrees <- get
       -- work in a fresh environment
       put Set.empty
       expr' <- reorder expr
       localFrees <- get
       -- merge with global frees
       modify (Set.union globalFrees)
       return (Canonical.Definition pattern expr' mType, Set.toList localFrees)


-- This also reorders the all of the sub-expressions in the Def list.
buildDefDict :: [Canonical.Def] -> State (Set.Set String) [(Canonical.Def, Int, [Int])]
buildDefDict defs =
  do pdefsDeps <- mapM reorderAndGetDependencies defs
     return $ realDeps (addKey pdefsDeps)

  where
    addKey :: [(Canonical.Def, [String])] -> [(Canonical.Def, Int, [String])]
    addKey = zipWith (\n (pdef,deps) -> (pdef,n,deps)) [0..]

    variableToKey :: (Canonical.Def, Int, [String]) -> [(String, Int)]
    variableToKey (Canonical.Definition pattern _ _, key, _) =
        [ (var, key) | var <- Set.toList (P.boundVars pattern) ]

    variableToKeyMap :: [(Canonical.Def, Int, [String])] -> Map.Map String Int
    variableToKeyMap pdefsDeps =
        Map.fromList (concatMap variableToKey pdefsDeps)

    realDeps :: [(Canonical.Def, Int, [String])] -> [(Canonical.Def, Int, [Int])]
    realDeps pdefsDeps = map convert pdefsDeps
        where
          varDict = variableToKeyMap pdefsDeps
          convert (pdef, key, deps) =
              (pdef, key, Maybe.mapMaybe (flip Map.lookup varDict) deps)
