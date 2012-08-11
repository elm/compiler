
module Types.Unify (unify) where

import Control.Arrow (second)
import Control.Monad (liftM)
import Data.List (foldl')
import qualified Data.Set as Set
import qualified Data.Map as Map
import Guid
import Types
import Types.Constrain
import Types.Substitutions

--import System.IO.Unsafe

prints xs v = v --} unsafePerformIO (putStrLn "----------" >> mapM print xs) `seq` v

unify hints modul = run $ do
  (escapees, cs) <- constrain hints modul
  subs <- solver cs Map.empty
  prints cs $ return ((,) escapees `liftM` subs)

eq ctx t1 t2 = Context ctx (t1 :=: t2)

solver [] subs = prints (Map.toList subs) $ return $ Right subs

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
                   [t] -> solver (map (cSub x t . cSub y t) cs) $ setXY t subs
                   _   -> solver cs $ setXY (Super ts) subs
          (Just (Super xts), _) ->
              solver (map (cSub y (VarT x)) cs) $ Map.insert y (VarT x) subs
          (_, _) ->
              solver (map (cSub x (VarT y)) cs) $ Map.insert x (VarT y) subs
solver (Context ctx (VarT x :=: t) : cs) subs =
    case Map.lookup x subs of
      Nothing -> solver (map (cSub x t) cs) . Map.map (tSub x t) $ Map.insert x t subs
      Just (Super ts) ->
          let ts' = Set.intersection ts (Set.singleton t) in
          case Set.toList ts' of
            []   -> solver (Context ctx (t :<: Super ts) : cs) subs
            [t'] -> solver (map (cSub x t') cs) $ Map.insert x t' subs
            _    -> solver cs $ Map.insert x (Super ts') subs
      Just t' -> solver (Context ctx (t' :=: t) : cs) subs
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
            [t]  -> solver (map (cSub x t) cs) $ Map.insert x t subs
            ts'' -> solver cs $ Map.insert x (Super $ Set.fromList ts'') subs

solver (Context ctx (ADT "List" [t] :<: Super ts) : cs) subs
    | any f (Set.toList ts) = solver cs subs
    | otherwise = subtypeError ("aoeu" ++ ctx) (ADT "List" [t]) (Super ts)
        where f (ADT "List" [VarT _]) = True
              f (ADT "List" [t']) = t == t'
              f _ = False

solver (Context ctx (t :<: Super ts) : cs) subs
    | Set.member t ts = solver cs subs
    | otherwise = subtypeError ("htns" ++ ctx) t (Super ts)

solver (Context ctx (x :<<: s) : cs) subs
    | any (\(Context _ c) -> x `elem` cFreeVars c) cs =
        do cs' <- concat `liftM` mapM (schemeSub x s) cs
           prints cs' $ solver cs' subs
    | otherwise =
        do (t,cs') <- concretize s
           let cs'' = (cs ++ Context ctx (VarT x :=: t) : map (extendCtx ctx) cs')
           prints cs'' $ solver cs'' subs


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
