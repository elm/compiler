
module Unify where

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

solver [] subs = return $ Right subs

--------  Destruct Type-constructors  --------

solver ((t1@(ADT n1 ts1) :=: t2@(ADT n2 ts2)) : cs) subs =
    if n1 /= n2 then uniError t1 t2 else
        solver (zipWith (:=:) ts1 ts2 ++ cs) subs
solver ((LambdaT t1 t2 :=: LambdaT t1' t2') : cs) subs =
    solver ([ t1 :=: t1', t2 :=: t2' ] ++ cs) subs

--------  Type-equality  --------

solver ((VarT x :=: t) : cs) subs =
    solver (map (cSub x t) cs) . map (second $ tSub x t) $ (x,t):subs
solver ((t :=: VarT x) : cs) subs = solver ((VarT x :=: t) : cs) subs
solver ((t1 :=: t2) : cs) subs =
    if t1 /= t2 then uniError t1 t2 else solver cs subs

--------  subtypes  --------

solver (c@(VarT x :<: SuperType n ts) : cs) subs
    | all isSuper cs = solver ((VarT x :=: ADT n []) : cs) subs
    | otherwise = solver (cs ++ [c]) subs
    where isSuper (VarT _ :<: _) = True
          isSuper _ = False
solver ((t@(ADT n1 []) :<: st@(SuperType n2 ts)) : cs) subs
    | n1 == n2 || Set.member t ts = solver cs subs
    | otherwise = return . Left $ "Type error: " ++ show t ++
                                  " is not a subtype of " ++ show st
solver ((t :<: st@(SuperType n ts)) : cs) subs
    | Set.member t ts = solver cs subs
    | otherwise = return . Left $ "Type error: " ++ show t ++
                                  " is not a subtype of " ++ show st

solver ((t1 :<<: Forall xs cs' t2) : cs) subs = do
  pairs <- mapM (\x -> liftM ((,) x . VarT) guid) xs
  let t2'  = foldr (uncurry tSub) t2 pairs
  let cs'' = foldr (\(k,v) -> map (cSub k v)) cs' pairs
  solver ((t1 :=: t2') : cs'' ++ cs) subs


cSub k v (t1 :=: t2) = force $ tSub k v t1 :=: tSub k v t2
cSub k v (t :<: super) = force $ tSub k v t :<: super
cSub k v (t :<<: poly) = force $ tSub k v t :<<: poly

tSub k v t@(VarT x) = if k == x then v else t
tSub k v (LambdaT t1 t2) = force $ LambdaT (tSub k v t1) (tSub k v t2)
tSub k v (ADT name ts) = ADT name (map (force . tSub k v) ts)
tSub _ _ t = t

uniError t1 t2 =
    return . Left $ "Type error: " ++ show t1 ++ " is not equal to " ++ show t2

force x = x `deepseq` x

instance NFData Constraint where
  rnf (t1 :=: t2) = t1 `deepseq` t2 `deepseq` ()
  rnf (t :<: _)   = t `deepseq` ()
  rnf (t :<<: _)  = t `deepseq` ()

instance NFData Type where
  rnf (LambdaT t1 t2) = t1 `deepseq` t2 `deepseq` ()
  rnf (ADT _ ts) =  foldl' (\acc x -> x `deepseq` acc) () ts
  rnf t = t `seq` ()
