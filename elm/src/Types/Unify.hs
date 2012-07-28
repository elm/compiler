
module Unify (unify) where

import Constrain
import Control.Arrow (second)
import Control.Monad (liftM)
import Data.List (foldl')
import qualified Data.Set as Set
import Guid
import Types

import Control.DeepSeq (NFData (..), deepseq)

unify hints expr = run $ do
  cs <- constrain hints expr
  solver cs []

eq ctx t1 t2 = Context ctx (t1 :=: t2)

solver [] subs = return $ Right subs

--------  Destruct Type-constructors  --------

solver ((Context ctx (t1@(ADT n1 ts1) :=: t2@(ADT n2 ts2))) : cs) subs =
    if n1 /= n2 then uniError ctx t1 t2 else
        solver (zipWith (eq ctx) ts1 ts2 ++ cs) subs
solver ((Context ctx (LambdaT t1 t2 :=: LambdaT t1' t2')) : cs) subs =
    solver ([ eq ctx t1 t1', eq ctx t2 t2' ] ++ cs) subs

--------  Type-equality  --------

solver ((Context ctx (VarT x :=: t)) : cs) subs =
    solver (map (cSub x t) cs) . map (second $ tSub x t) $ (x,t):subs
solver ((Context ctx (t :=: VarT x)) : cs) subs = solver ((Context ctx (VarT x :=: t)) : cs) subs
solver ((Context ctx (t1 :=: t2)) : cs) subs
    | t1 == t2  = solver cs subs
    | otherwise = uniError ctx t1 t2

--------  subtypes  --------

solver (c@(Context ctx (VarT x :<: SuperType n ts)) : cs) subs
    | all isSuper cs = solver ((Context ctx (VarT x :=: ADT n [])) : cs) subs
    | otherwise = solver (cs ++ [c]) subs
    where isSuper (Context _ (VarT _ :<: _)) = True
          isSuper _ = False
solver ((Context ctx (t@(ADT n1 []) :<: st@(SuperType n2 ts))) : cs) subs
    | n1 == n2 || Set.member t ts = solver cs subs
    | otherwise = return . Left $ "Type error: " ++ show t ++
                                  " is not a " ++ show st ++
                                  " in context " ++ show ctx
solver ((Context ctx (t@(ADT "List" [_]) :<: st@(SuperType n' ts'))) : cs) subs
    | any f (Set.toList ts') || Set.member t ts' = solver cs subs
    | otherwise = return . Left $ "Type error: " ++ show t ++
                                  " is not a " ++ show st ++
                                  " in context " ++ show ctx
        where f (ADT "List" [VarT _]) = True
              f _ = False

solver ((Context ctx (t :<: st@(SuperType n ts))) : cs) subs
    | Set.member t ts = solver cs subs
    | otherwise = return . Left $ "Type error: " ++ show t ++
                                  " is not a " ++ show st

solver ((Context ctx (t1 :<<: Forall xs cs' t2)) : cs) subs = do
  pairs <- mapM (\x -> liftM ((,) x . VarT) guid) xs
  let t2'  = foldr (uncurry tSub) t2 pairs
  let cs'' = foldr (\(k,v) -> map (cSub k v)) (map (Context ctx) cs') pairs
  let constrs = ((Context ctx (t1 :=: t2')) : cs'' ++ cs)
  solver constrs subs


cSub k v (Context c (t1 :=: t2)  ) = force . Context c $ tSub k v t1 :=: tSub k v t2
cSub k v (Context c (t :<: super)) = force . Context c $ tSub k v t :<: super
cSub k v (Context c (t :<<: poly)) = force . Context c $ tSub k v t :<<: poly

tSub k v t@(VarT x) = if k == x then v else t
tSub k v (LambdaT t1 t2) = force $ LambdaT (tSub k v t1) (tSub k v t2)
tSub k v (ADT name ts) = ADT name (map (force . tSub k v) ts)

uniError ctx t1 t2 =
    return . Left $ "Type error: " ++ show t1 ++
                    " is not equal to " ++ show t2 ++
                    " in context " ++ show ctx

force x = x `deepseq` x

instance (NFData a) => NFData (Context c a) where
  rnf (Context _ a) = a `deepseq` ()

instance NFData Constraint where
  rnf (t1 :=: t2) = t1 `deepseq` t2 `deepseq` ()
  rnf (t :<: _)   = t `deepseq` ()
  rnf (t :<<: _)  = t `deepseq` ()

instance NFData Type where
  rnf (LambdaT t1 t2) = t1 `deepseq` t2 `deepseq` ()
  rnf (ADT _ ts) =  foldl' (\acc x -> x `deepseq` acc) () ts
  rnf t = t `seq` ()
