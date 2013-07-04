
module Transform.SortDefinitions (boundVars) where

import Control.Monad.State
import qualified Data.Set as Set
import qualified Data.Map as Map
import SourceSyntax.Everything


boundVars :: Pattern -> Set.Set String
boundVars pattern =
    case pattern of
      PVar x -> Set.singleton x
      PAlias x p -> Set.insert x (boundVars p)
      PData _ ps -> Set.unions (map boundVars ps)
      PRecord fields -> Set.fromList fields
      PAnything -> Set.empty
      PLiteral _ -> Set.empty

free x = modify (Set.insert x)
bound boundVars = modify (\freeVars -> Set.difference freeVars boundVars)

reorderAndGetDependencies pattern expr =
    do globalFrees <- get
       expr' <- reorder expr
       localFrees <- get
       modify (Set.union globalFrees)
       let addDep var deps = Map.insert var localFrees deps
           dependencies = Set.foldr addDep Map.empty (boundVars pattern)
       return (expr', dependencies)

reorder :: LExpr t v -> State (Set.Set String) (LExpr t v)
reorder lexpr@(L a b expr) =
    L a b `liftM`
    case expr of
{--
      -- Actually do some reordering
      Let defs body ->
          do -- swap in a fresh environment

             -- swap in the old environment
             body' <- reorder body
             bound (boundVars (patterns defs))
             Let `liftM` defs' `ap` body'

          where
            patterns = flip concatMap defs $
                       case def of
                         Def pattern _ -> [pattern]
                         TypeAnnotation _ _ -> []
--}
      -- Be careful adding and restricting freeVars
      Var x -> free x >> return expr

      Lambda p e ->
          uncurry Lambda `liftM` bindingReorder (p,e)

      Binop op e1 e2 ->
          do free op
             Binop op `liftM` reorder e1 `ap` reorder e2

      Case e cases ->
          Case `liftM` reorder e `ap` mapM bindingReorder cases

      Data name es ->
          do free name
             Data name `liftM` mapM reorder es

      -- Just pipe the reorder though
      Literal _ -> return expr

      Range e1 e2 ->
          Range `liftM` reorder e1 `ap` reorder e2

      ExplicitList es ->
          ExplicitList `liftM` mapM reorder es

      App e1 e2 ->
          App `liftM` reorder e1 `ap` reorder e2

      MultiIf branches ->
          MultiIf `liftM` mapM reorderPair branches

      Access e lbl ->
          Access `liftM` reorder e `ap` return lbl

      Remove e lbl ->
          Remove `liftM` reorder e `ap` return lbl

      Insert e lbl v ->
          Insert `liftM` reorder e `ap` return lbl `ap` reorder v

      Modify e fields ->
          Modify `liftM` reorder e `ap` mapM reorderField fields

      Record fields ->
          Record `liftM` mapM reorderField fields

      Markdown _ -> return expr

reorderField (label, expr) =
    (,) label `liftM` reorder expr

reorderPair (e1,e2) =
    (,) `liftM` reorder e1 `ap` reorder e2

bindingReorder :: (Pattern, LExpr t v) -> State (Set.Set String) (Pattern, LExpr t v)
bindingReorder (pattern,expr) =
    do expr' <- reorder expr
       bound (boundVars pattern)
       return (pattern, expr')

