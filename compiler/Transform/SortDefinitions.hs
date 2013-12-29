
module Transform.SortDefinitions (sortDefs, boundVars, flattenLets) where

import Control.Monad.State
import Control.Applicative ((<$>))
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified SourceSyntax.Type as ST
import SourceSyntax.Expression
import SourceSyntax.Location
import SourceSyntax.Pattern
import qualified Data.Graph as Graph
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

boundVars :: Pattern -> Set.Set String
boundVars pattern =
    case pattern of
      PVar x -> Set.singleton x
      PAlias x p -> Set.insert x (boundVars p)
      PData _ ps -> Set.unions (map boundVars ps)
      PRecord fields -> Set.fromList fields
      PAnything -> Set.empty
      PLiteral _ -> Set.empty

ctors :: Pattern -> [String]
ctors pattern =
    case pattern of
      PVar x -> []
      PAlias x p -> ctors p
      PData ctor ps -> ctor : concatMap ctors ps
      PRecord fields -> []
      PAnything -> []
      PLiteral _ -> []

free :: String -> State (Set.Set String) ()
free x = modify (Set.insert x)

bound :: Set.Set String -> State (Set.Set String) ()
bound boundVars = modify (\freeVars -> Set.difference freeVars boundVars)

sortDefs :: LExpr t v -> LExpr t v
sortDefs expr = evalState (reorder expr) Set.empty

flattenLets defs lexpr@(L _ expr) =
    case expr of
      Let ds body -> flattenLets (defs ++ ds) body
      _ -> (defs, lexpr)


reorder :: LExpr t v -> State (Set.Set String) (LExpr t v)
reorder lexpr@(L s expr) =
    L s `liftM`
    case expr of
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

      Markdown uid md es -> Markdown uid md <$> mapM reorder es

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
             let getPatterns def =
                     case def of
                       Def pattern _ -> pattern
                       TypeAnnotation name _ -> PVar name
             forM (map getPatterns defs) $ \pattern -> do
                bound (boundVars pattern)
                mapM free (ctors pattern)

             let addDefs ds bod = L s (Let (concatMap toDefs ds) bod)
                 L _ let' = foldr addDefs body' defss

             return let'

          where
            toDefs def =
                case def of
                  (pattern, expr, Nothing) -> [ Def pattern expr ]
                  (PVar name, expr, Just tipe) ->
                      [ TypeAnnotation name tipe, Def (PVar name) expr ]
                  _ -> error $ unlines 
                       [ "The impossible occurred."
                       , "Please report an issue at <https://github.com/evancz/Elm/issues>."
                       , "Be very descriptive because something quite weird probably happened." ]

reorderField (label, expr) =
    (,) label `liftM` reorder expr

reorderPair (e1,e2) =
    (,) `liftM` reorder e1 `ap` reorder e2

bindingReorder :: (Pattern, LExpr t v) -> State (Set.Set String) (Pattern, LExpr t v)
bindingReorder (pattern,expr) =
    do expr' <- reorder expr
       bound (boundVars pattern)
       mapM free (ctors pattern)
       return (pattern, expr')


type PDef t v = (Pattern, LExpr t v, Maybe ST.Type)

reorderAndGetDependencies :: PDef t v -> State (Set.Set String) (PDef t v, [String])
reorderAndGetDependencies (pattern, expr, mType) =
    do globalFrees <- get
       -- work in a fresh environment
       put Set.empty
       expr' <- reorder expr
       localFrees <- get
       -- merge with global frees
       modify (Set.union globalFrees)
       return ((pattern, expr', mType), Set.toList localFrees)


-- This also reorders the all of the sub-expressions in the Def list.
buildDefDict :: [Def t v] -> State (Set.Set String) [(PDef t v, Int, [Int])]
buildDefDict defs =
  do pdefsDeps <- mapM reorderAndGetDependencies (getPDefs defs)
     return $ realDeps (addKey pdefsDeps)

  where
    getPDefs :: [Def t v] -> [PDef t v]
    getPDefs defs = map (\(p,(e,t)) -> (p,e,t)) $
                    Map.toList $ go defs Map.empty Map.empty
      where
        go [] ds ts =
            Map.unions [ Map.difference ds ts
                       , Map.intersectionWith (\(e,_) t -> (e,Just t)) ds ts ]
        
        go (def:defs) ds ts =
            case def of
              Def p e -> go defs (Map.insert p (e, Nothing) ds) ts
              TypeAnnotation name tipe -> go defs ds (Map.insert (PVar name) tipe ts)

    addKey :: [(PDef t v, [String])] -> [(PDef t v, Int, [String])]
    addKey = zipWith (\n (pdef,deps) -> (pdef,n,deps)) [0..]

    variableToKey :: (PDef t v, Int, [String]) -> [(String, Int)]
    variableToKey ((pattern, _, _), key, _) =
        [ (var, key) | var <- Set.toList (boundVars pattern) ]

    variableToKeyMap :: [(PDef t v, Int, [String])] -> Map.Map String Int
    variableToKeyMap pdefsDeps =
        Map.fromList (concatMap variableToKey pdefsDeps)

    realDeps :: [(PDef t v, Int, [String])] -> [(PDef t v, Int, [Int])]
    realDeps pdefsDeps = map convert pdefsDeps
        where
          varDict = variableToKeyMap pdefsDeps
          convert (pdef, key, deps) =
              (pdef, key, Maybe.mapMaybe (flip Map.lookup varDict) deps)

