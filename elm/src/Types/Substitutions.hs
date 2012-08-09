
module Types.Substitutions (cSub
                           ,tSub
                           ,cSubNoContext
                           ,cFreeVars
                           ,schemeSub
                           ,concretize
                           ,rescheme
                           ,generalize) where

import Control.DeepSeq (NFData (..), deepseq)
import Control.Monad (liftM)
import Data.List (foldl')
import qualified Data.Set as Set
import Guid
import Types

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

tSub k v t@(VarT x) = if k == x then v else t
tSub k v (LambdaT t1 t2) = force $ LambdaT (tSub k v t1) (tSub k v t2)
tSub k v (ADT name ts) = ADT name (map (force . tSub k v) ts)
tSub k v (Super ts) = Super ts

schemeSubHelp k s c t1 t2 relation = do
  (t1',cs1) <- f1 ; (t2',cs2) <- f2
  return (Context c (relation t1' t2') : cs1 ++ cs2)
    where f1 | hasVar k t1 = do (v, cs) <- concretize s; return (tSub k v t1, cs)
             | otherwise = return (t1, [])
          f2 | hasVar k t2 = do (v, cs) <- concretize s; return (tSub k v t2, cs)
             | otherwise = return (t2, [])

schemeSub k s (Context ctx (t1 :=: t2)) = schemeSubHelp k s ctx t1 t2 (:=:)
schemeSub k s (Context ctx (t1 :<: t2)) = schemeSubHelp k s ctx t1 t2 (:<:)
schemeSub k s (Context ctx c@(x :<<: Forall cxs ccs ctipe))
    | k `elem` cxs || k `notElem` cFreeVars c = return [Context ctx c]
    | otherwise = do
          Forall xs cs tipe <- rescheme s
          let constraints = (map (cSub k tipe) $ cs ++ ccs)
          let c' = x :<<: Forall (cxs ++ xs) constraints (tSub k tipe ctipe)
          return [ Context ctx c' ]

hasVar v (VarT x) = v == x
hasVar v (LambdaT t1 t2) = hasVar v t1 || hasVar v t2
hasVar v (ADT _ ts) = any (hasVar v) ts
hasVar v (Super _ ) = False

cSub k v (Context ctx c) = Context ctx (cSubNoContext k v c)

cSubNoContext k v (t1 :=: t2) = force $ tSub k v t1 :=: tSub k v t2
cSubNoContext k v (t :<: super) = force $ tSub k v t :<: super
cSubNoContext k v (x :<<: poly@(Forall vs cs tipe)) = force $ x :<<: poly'
    where poly' | k `elem` vs = poly
                | otherwise = let cs' = map (cSub k v) cs in
                              Forall vs cs' (tSub k v tipe)

concretize (Forall xs cs t) = do
  pairs <- mapM (\x -> liftM ((,) x) guid) xs
  return ( foldl' (\t'  (k,v) -> tSub k (VarT v) t) t pairs
         , foldl' (\cs' (k,v) -> map (cSub k $ VarT v) cs') cs pairs )

rescheme :: Scheme -> GuidCounter Scheme
rescheme (Forall xs cs t) = do
  pairs <- mapM (\x -> liftM ((,) x) guid) xs
  let t'  = foldl' (\t'  (k,v) -> tSub k (VarT v) t') t pairs
  let cs' = foldl' (\cs' (k,v) -> map (cSub k $ VarT v) cs') cs pairs
  return $ Forall (map snd pairs) cs' t'

freeVars (VarT v) = [v]
freeVars (LambdaT t1 t2) = freeVars t1 ++ freeVars t2
freeVars (ADT _ ts) = concatMap freeVars ts
freeVars (Super _ ) = []

cFreeVars (t1 :=: t2) = freeVars t1 ++ freeVars t2
cFreeVars (t1 :<: t2) = freeVars t1 ++ freeVars t2
cFreeVars (x :<<: Forall xs cs t) = filter (`notElem` xs) frees
    where frees = concatMap (\(Context _ c) -> cFreeVars c) cs ++ freeVars t

decontext (Context _ c) = c

generalize :: [X] -> Scheme -> GuidCounter Scheme
generalize exceptions (Forall xs cs t) = rescheme (Forall (xs ++ frees) cs t)
    where allFrees = Set.fromList $ freeVars t ++ concatMap (cFreeVars . decontext) cs
          frees = Set.toList $ Set.difference allFrees (Set.fromList exceptions)

