
module Types.Solver (solver) where

import Control.Arrow (second)
import Control.Monad (liftM)
import Data.Either (lefts,rights)
import Data.List (foldl')
import Data.Maybe (isJust)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Guid
import Types.Types
import Types.Constrain
import Types.Substitutions


eq ctx t1 t2 = Context ctx (t1 :=: t2)

isSolved ss (Context _ (t1 :=: t2)) = t1 == t2
isSolved ss (Context _ (x :<<: _)) = isJust (lookup x ss)
isSolved ss c = False

crush (Forall xs cs t) =
    do subs <- solver cs Map.empty
       return $ do ss' <- subs
                   let ss  = Map.toList ss'
                       cs' = filter (not . isSolved ss) (subst ss cs)
                   return $ Forall xs cs' (subst ss t)

schemeSubHelp ctx x s t1 rltn t2 = do
  (t1',cs1) <- sub t1
  (t2',cs2) <- sub t2
  return (Context ctx (rltn t1' t2') : cs1 ++ cs2)
      where sub t | not (occurs x t) = return (t, [])
                  | otherwise = do (st, cs) <- concretize s
                                   return (subst [(x,st)] t, cs)

schemeSub x s c = do s' <- crush s
                     case s' of
                       Right s'' -> Right `liftM` schemeSub' x s'' c
                       Left err  -> return $ Left err

schemeSub' x s (Context ctx (t1 :=: t2)) = schemeSubHelp ctx x s t1 (:=:) t2
schemeSub' x s (Context ctx (t1 :<: t2)) = schemeSubHelp ctx x s t1 (:<:) t2
schemeSub' x s c@(Context ctx (y :<<: Forall cxs ccs ctipe))
    | not (occurs x c) = return [c]
    | otherwise =
        do Forall xs cs tipe <- rescheme s
           let ss = [(x,tipe)]
               constraints = subst ss (cs ++ ccs)
               c' = y :<<: Forall (cxs ++ xs) constraints (subst ss ctipe)
           return [ Context ctx c' ]


solver :: [Context String Constraint]
       -> Map.Map X Type
       -> GuidCounter (Either String (Map.Map X Type))
solver [] subs = return $ Right subs

--------  Destruct Type-constructors  --------

solver ((Context ctx (t1@(ADT n1 ts1) :=: t2@(ADT n2 ts2))) : cs) subs =
    if n1 /= n2 then uniError ctx t1 t2 else
        solver (zipWith (eq ctx) ts1 ts2 ++ cs) subs
solver ((Context ctx (LambdaT t1 t2 :=: LambdaT t1' t2')) : cs) subs =
    solver ([ eq ctx t1 t1', eq ctx t2 t2' ] ++ cs) subs

--------  Type-equality  --------

solver (Context ctx (VarT x :=: VarT y) : cs) subs
    | x == y    = solver cs subs
    | otherwise =
        case (Map.lookup x subs, Map.lookup y subs) of
          (Just (Super xts), Just (Super yts)) ->
             let ts = Set.intersection xts yts
                 setXY t = Map.insert x t . Map.insert y t
             in  case Set.toList ts of
                   []  -> unionError ctx xts yts
                   [t] -> let cs1 = subst [(x,t),(y,t)] cs in
                          cs1 `seq` solver cs1 (setXY t subs)
                   _   -> solver cs $ setXY (Super ts) subs
          (Just (Super xts), _) ->
              let cs2 = subst [(y,VarT x)] cs in
              solver cs2 $ Map.insert y (VarT x) subs
          (_, _) ->
              let cs3 = subst [(x,VarT y)] cs in
              solver cs3 $ Map.insert x (VarT y) subs
solver (Context ctx (VarT x :=: t) : cs) subs = do
  if x `occurs` t then occursError ctx (VarT x) t else
      (case Map.lookup x subs of
         Nothing -> let cs4 = subst [(x,t)] cs in
                    solver cs4 . Map.map (subst [(x,t)]) $ Map.insert x t subs
         Just (Super ts) ->
             let ts' = Set.intersection ts (Set.singleton t) in
             case Set.toList ts' of
               []   -> solver (Context ctx (t :<: Super ts) : cs) subs
               [t'] -> let cs5 = subst [(x,t)] cs in
                       solver cs5 $ Map.insert x t' subs
               _    -> solver cs $ Map.insert x (Super ts') subs
         Just t' -> solver (Context ctx (t' :=: t) : cs) subs
      )
solver ((Context ctx (t :=: VarT x)) : cs) subs =
    solver ((Context ctx (VarT x :=: t)) : cs) subs
solver ((Context ctx (t1 :=: t2)) : cs) subs
    | t1 == t2  = solver cs subs
    | otherwise = uniError ctx t1 t2


--------  subtypes  --------

solver (Context ctx (VarT x :<: Super ts) : cs) subs =
    case Map.lookup x subs of
      Nothing -> solver cs $ Map.insert x (Super ts) subs
      Just (Super ts') ->
          case Set.toList $ Set.intersection ts ts' of
            []   -> unionError ctx ts ts'
            [t]  -> solver (subst [(x,t)] cs) $ Map.insert x t subs
            ts'' -> solver cs $ Map.insert x (Super $ Set.fromList ts'') subs

solver (Context ctx (ADT "List" [t] :<: Super ts) : cs) subs
    | any f (Set.toList ts) = solver cs subs
    | otherwise = subtypeError ctx (ADT "List" [t]) (Super ts)
        where f (ADT "List" [VarT _]) = True
              f (ADT "List" [t']) = t == t'
              f _ = False

solver (Context ctx (t :<: Super ts) : cs) subs
    | Set.member t ts = solver cs subs
    | otherwise = subtypeError ctx t (Super ts)

solver (Context ctx (x :<<: s) : cs) subs
    | any (occurs x) cs =
        do css <- mapM (schemeSub x s) cs
           case lefts css of
             err : _ -> return $ Left err
             [] -> solver (concat (rights css)) subs
    | otherwise =
        do (t,cs7) <- concretize s
           let cs'' = (cs ++ Context ctx (VarT x :=: t) : map (extendCtx ctx) cs7)
           solver cs'' subs

occursError ctx t1 t2 =
    return . Left $ "Type error: Occurs check: cannot construct the infinite type: " ++ show t1 ++
                    " = " ++ show t2 ++
                    " in context " ++ ctx
uniError ctx t1 t2 =
    return . Left $ "Type error: " ++ show t1 ++
                    " is not equal to " ++ show t2 ++
                    " in context " ++ ctx
unionError ctx ts ts' =
    return . Left $ concat [ "Type error: There are no types in both "
                           , show (Super ts), " and ", show (Super ts')
                           , " in context ", ctx ]
subtypeError ctx t s =
    return . Left $ concat [ "Type error: ", show t, " is not a ", show s
                           , " in context ", ctx ]

