{-# OPTIONS_GHC -Wall #-}
module Transform.SortDefinitions (sortDefs) where

import Control.Monad.State
import Control.Applicative ((<$>),(<*>))
import qualified Data.Map as Map
import SourceSyntax.Expression
import SourceSyntax.Location
import qualified SourceSyntax.Pattern as P
import qualified Data.Graph as Graph
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe

ctors :: P.Pattern -> [String]
ctors pattern =
    case pattern of
      P.PVar _ -> []
      P.PAlias _ p -> ctors p
      P.PData ctor ps -> ctor : concatMap ctors ps
      P.PRecord _ -> []
      P.PAnything -> []
      P.PLiteral _ -> []

free :: String -> State (Set.Set String) ()
free x = modify (Set.insert x)

bound :: Set.Set String -> State (Set.Set String) ()
bound boundVars = modify (\freeVars -> Set.difference freeVars boundVars)

sortDefs :: LExpr -> LExpr
sortDefs expr = evalState (reorder expr) Set.empty

reorder :: LExpr -> State (Set.Set String) LExpr
reorder (L s expr) =
    L s <$>
    case expr of
      -- Be careful adding and restricting freeVars
      Var x -> free x >> return expr

      Lambda p e ->
          uncurry Lambda <$> bindingReorder (p,e)

      Binop op e1 e2 ->
          do free op
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

      Markdown uid md es -> Markdown uid md <$> mapM reorder es

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
             forM_ defs $ \(Definition pattern _ _) -> do
                bound (P.boundVars pattern)
                mapM free (ctors pattern)

             let L _ let' = foldr (\ds bod -> L s (Let ds bod)) body' defss

             return let'

bindingReorder :: (P.Pattern, LExpr) -> State (Set.Set String) (P.Pattern, LExpr)
bindingReorder (pattern,expr) =
    do expr' <- reorder expr
       bound (P.boundVars pattern)
       mapM_ free (ctors pattern)
       return (pattern, expr')


reorderAndGetDependencies :: Def -> State (Set.Set String) (Def, [String])
reorderAndGetDependencies (Definition pattern expr mType) =
    do globalFrees <- get
       -- work in a fresh environment
       put Set.empty
       expr' <- reorder expr
       localFrees <- get
       -- merge with global frees
       modify (Set.union globalFrees)
       return (Definition pattern expr' mType, Set.toList localFrees)


-- This also reorders the all of the sub-expressions in the Def list.
buildDefDict :: [Def] -> State (Set.Set String) [(Def, Int, [Int])]
buildDefDict defs =
  do pdefsDeps <- mapM reorderAndGetDependencies defs
     return $ realDeps (addKey pdefsDeps)

  where
    addKey :: [(Def, [String])] -> [(Def, Int, [String])]
    addKey = zipWith (\n (pdef,deps) -> (pdef,n,deps)) [0..]

    variableToKey :: (Def, Int, [String]) -> [(String, Int)]
    variableToKey (Definition pattern _ _, key, _) =
        [ (var, key) | var <- Set.toList (P.boundVars pattern) ]

    variableToKeyMap :: [(Def, Int, [String])] -> Map.Map String Int
    variableToKeyMap pdefsDeps =
        Map.fromList (concatMap variableToKey pdefsDeps)

    realDeps :: [(Def, Int, [String])] -> [(Def, Int, [Int])]
    realDeps pdefsDeps = map convert pdefsDeps
        where
          varDict = variableToKeyMap pdefsDeps
          convert (pdef, key, deps) =
              (pdef, key, Maybe.mapMaybe (flip Map.lookup varDict) deps)
