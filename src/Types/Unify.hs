
module Unify where

import Constrain
import Control.Arrow (second)
import Types

unify expr = ( constrain expr, solver (constrain expr) [] )

solver [] subs = Right subs

solver ((t1 :<: t2) : cs) subs = solver ((t1 :=: t2) : cs) subs

solver ((ADT n1 ts1 :=: ADT n2 ts2) : cs) subs =
    if n1 /= n2 then Left "unification error" else
        solver (zipWith (:=:) ts1 ts2 ++ cs) subs

solver ((LambdaT t1 t2 :=: LambdaT t1' t2') : cs) subs =
    solver ([ t1 :=: t1', t2 :=: t2' ] ++ cs) subs

solver ((VarT x :=: t) : cs) subs =
    solver (map (cSub x t) cs) . map (second $ tSub x t) $ (x,t):subs

solver ((t :=: VarT x) : cs) subs =
    solver (map (cSub x t) cs) . map (second $ tSub x t) $ (x,t):subs

solver ((t1 :=: t2) : cs) subs =
    if t1 /= t2 then Left $ show t1 ++ " is not equal to " ++ show t2 else
        solver cs subs


cSub k v (t1 :=: t2) = tSub k v t1 :=: tSub k v t2
cSub k v (t1 :<: t2) = tSub k v t1 :=: tSub k v t2

tSub k v (VarT x) = if k == x then v else (VarT x)
tSub k v (LambdaT t1 t2) = LambdaT (tSub k v t1) (tSub k v t2)
tSub _ _ t = t