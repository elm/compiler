
module Unify where

import Constrain
import Control.Arrow (second)
import Control.Monad (liftM)
import qualified Data.Set as Set
import Types

import Guid

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
solver ((t :=: VarT x) : cs) subs =
    solver (map (cSub x t) cs) . map (second $ tSub x t) $ (x,t):subs
solver ((t1 :=: t2) : cs) subs =
    if t1 /= t2 then uniError t1 t2 else solver cs subs

--------  subtypes  --------

solver ((t1 :<: t2) : cs) subs = do
  let f x = do y <- guid ; return (x,VarT y)
  t2' <- foldr (uncurry tSub) t2 `liftM` (mapM f . Set.toList $ getVars t2)
  solver ((t1 :=: t2') : cs) subs


cSub k v (t1 :=: t2) = tSub k v t1 :=: tSub k v t2
cSub k v (t1 :<: t2) = tSub k v t1 :<: tSub k v t2

tSub k v (VarT x) = if k == x then v else (VarT x)
tSub k v (LambdaT t1 t2) = LambdaT (tSub k v t1) (tSub k v t2)
tSub k v (ADT name ts) = ADT name (map (tSub k v) ts)
tSub _ _ t = t

getVars (VarT x)        = Set.singleton x
getVars (LambdaT t1 t2) = Set.union (getVars t1) (getVars t2)
getVars (ADT name ts)   = Set.unions $ map getVars ts
getVars _               = Set.empty

uniError t1 t2 =
    return . Left $ "Type error: " ++ show t1 ++ " is not equal to " ++ show t2