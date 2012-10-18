
module Types.Substitutions (cSub
                           ,tSub
                           ,cSubNoContext
                           ,hasVarC
                           ,freeVars
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
a ++++ b = foldl' (\tl hd -> hd : tl) b a
map' f xs = map (\x -> let v = f x in v `seq` v) xs

instance (NFData a) => NFData (Context c a) where
  rnf (Context _ a) = a `deepseq` ()

instance NFData Scheme where
  rnf (Forall vs cs t) = vs `deepseq` cs `deepseq` t `deepseq` ()

instance NFData Constraint where
  rnf (t1 :=: t2) = t1 `deepseq` t2 `deepseq` ()
  rnf (t1 :<: t2) = t1 `deepseq` t2 `deepseq` ()
  rnf (t :<<: s)  = t `deepseq` s `deepseq` ()

instance NFData Type where
  rnf (LambdaT t1 t2) = t1 `deepseq` t2 `deepseq` ()
  rnf (ADT _ ts) =  foldl' (\acc x -> x `deepseq` acc) () ts
  rnf t = t `seq` ()

tSub k v t@(VarT x) = if k == x then v else t
tSub k v (LambdaT t1 t2) = tSub k v t1 `seq` tSub k v t2 `seq` LambdaT (tSub k v t1) (tSub k v t2)
tSub k v (ADT name ts) = map' (tSub k v) ts `seq` ADT name (map' (tSub k v) ts)
tSub k v (Super ts) = Super ts

schemeSubHelp k s c t1 t2 relation = do
  (t1',cs1) <- f t1
  (t2',cs2) <- f t2
  return $ Context c (relation t1' t2') : cs1 ++++ cs2
    where f t | hasVar k t = do (v, cs) <- concretize s; return (tSub k v t, cs)
              | otherwise  = return (t, [])

schemeSub k s (Context ctx (t1 :=: t2)) = t1 `seq` t2 `seq` schemeSubHelp k s ctx t1 t2 (:=:)
schemeSub k s (Context ctx (t1 :<: t2)) = t1 `seq` t2 `seq` schemeSubHelp k s ctx t1 t2 (:<:)
schemeSub k s c@(Context ctx (x :<<: Forall cxs ccs ctipe)) =
    if not $ hasVarC k c then return [c]
    else do
      Forall xs cs tipe <- rescheme s
      let constraints = map' (cSub k tipe) $ cs ++ ccs
      let c' = x :<<: Forall (cxs ++ xs) constraints (tSub k tipe ctipe)
      return [ Context ctx c' ]

hasVar v (VarT x) = v == x
hasVar v (LambdaT t1 t2) = force (hasVar v t1 || hasVar v t2)
hasVar v (ADT _ ts) = force $ any (hasVar v) ts
hasVar v (Super _ ) = False

cSub k v (Context ctx c) = out `deepseq` out
    where out = Context ctx (cSubNoContext' k v c)

cSubNoContext k v c = c' `deepseq` c'
    where c' = cSubNoContext' k v c

cSubNoContext' k v (t1 :=: t2) = let { t1' = tSub k v $! t1 ; t2' = tSub k v $! t2 } in
                                 t1' `seq` t2' `seq` t1' :=: t2'
cSubNoContext' k v (t :<: super) = let t' = tSub k v t in t' `seq` t' :<: super
cSubNoContext' k v (x :<<: poly@(Forall vs cs tipe)) = (x :<<: poly')
    where poly' | k `elem` vs = poly
                | otherwise = let tipe' = tSub k v tipe in
                              tipe' `deepseq` Forall vs (map' (cSub k v) cs) tipe'

concretize (Forall xs cs t) = do
  pairs <- zip xs `liftM` mapM (const guid) xs
  let tipe = foldl' (\t' (k,v) -> tSub k (VarT v) $! t') t pairs
  let f c = foldl' (\c (k,v) -> cSub k (VarT v) $! c) c $! pairs
  let cs' = f `seq` map' f cs
  tipe `deepseq` cs' `deepseq` return (tipe, cs')

rescheme :: Scheme -> GuidCounter Scheme
rescheme (Forall xs cs t) = do
  pairs <- zip xs `liftM` mapM (const guid) xs
  let tipe = foldl' (\t' (k,v) -> tSub k (VarT v) $! t') t pairs
  let fs = map (\(k,v) -> cSub k (VarT v)) pairs
  let cs' = map (\c -> foldl' (\c f -> f $! c) c fs) cs
  tipe `deepseq` cs' `deepseq` (return $ Forall (map' snd pairs) cs' tipe)

freeVars t = let fs = freeVars' t in fs `deepseq` fs

freeVars' (VarT v) = [v]
freeVars' (LambdaT t1 t2) = freeVars' t1 ++++ freeVars' t2
freeVars' (ADT _ ts) = concatMap freeVars' ts
freeVars' (Super _ ) = []

cFreeVars c = let fs = cFreeVars' c in fs `deepseq` fs

cFreeVars' (t1 :=: t2) = freeVars' t1 ++++ freeVars' t2
cFreeVars' (t1 :<: t2) = freeVars' t1 ++++ freeVars' t2
cFreeVars' (x :<<: Forall xs cs t) = filter (`notElem` xs) frees
    where frees = concatMap (\(Context _ c) -> cFreeVars' c) cs ++++ freeVars' t


hasVarC x (Context _ (t1 :=: t2)) = hasVar x t1 || hasVar x t2
hasVarC x (Context _ (t1 :<: t2)) = hasVar x t1 || hasVar x t2
hasVarC x (Context _ (y :<<: Forall xs cs t)) = x == y || hasVar x t || inCs
    where inCs = x `notElem` xs && any (hasVarC x) cs

decontext (Context _ c) = c

generalize :: [X] -> Scheme -> GuidCounter Scheme
generalize exceptions (Forall xs cs t) = rescheme (Forall (xs ++ frees) cs t)
    where allFrees = Set.fromList $ freeVars t ++ concatMap (cFreeVars . decontext) cs
          frees = Set.toList $ Set.difference allFrees (Set.fromList exceptions)

