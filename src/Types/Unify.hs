
module Unify where

import Constrain
import Control.Arrow (second)
import Types

unify expr = const expr `fmap` solver (constrain expr) []

solver [] subs = Right subs

--------  Destruct Type-constructors  --------

solver ((t1@(ADT n1 ts1) :<: t2@(ADT n2 ts2)) : cs) subs =
    solver cs subs
--    if n1 /= n2 then uniError t1 t2 else
--        solver (zipWith (:<:) ts1 ts2 ++ cs) subs

solver ((t1@(ADT n1 ts1) :=: t2@(ADT n2 ts2)) : cs) subs =
    solver cs subs
--    if n1 /= n2 then uniError t1 t2 else
--        solver (zipWith (:=:) ts1 ts2 ++ cs) subs

solver ((LambdaT t1 t2 :<: LambdaT t1' t2') : cs) subs =
    solver ([ t1 :<: t1', t2 :<: t2' ] ++ cs) subs

solver ((LambdaT t1 t2 :=: LambdaT t1' t2') : cs) subs =
    solver ([ t1 :=: t1', t2 :=: t2' ] ++ cs) subs

--------  subtypes  --------

solver ((t :<: VarT x) : cs) subs = solver cs' subs
    where cs' = if all isSubtype cs then (t :=: VarT x : cs) else (cs ++ [t :<: VarT x])
solver ((t1 :<: t2) : cs) subs = solver ((t1 :=: t2) : cs) subs

--------  Type-equality  --------

solver ((VarT x :=: t) : cs) subs =
    solver (map (cSub x t) cs) . map (second $ tSub x t) $ (x,t):subs
solver ((t :=: VarT x) : cs) subs =
    solver (map (cSub x t) cs) . map (second $ tSub x t) $ (x,t):subs
solver ((t1 :=: t2) : cs) subs =
    if t1 /= t2 then uniError t1 t2 else solver cs subs


cSub k v (t1 :=: t2) = tSub k v t1 :=: tSub k v t2
cSub k v (t1 :<: t2) = tSub k v t1 :<: tSub k v t2

tSub k v (VarT x) = if k == x then v else (VarT x)
tSub k v (LambdaT t1 t2) = LambdaT (tSub k v t1) (tSub k v t2)
tSub _ _ t = t

isSubtype (_ :<: _) = True
isSubtype     _     = False

uniError t1 t2 =
    Left $ "Type error: " ++ show t1 ++ " is not equal to " ++ show t2