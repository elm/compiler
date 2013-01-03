
module Types.Substitutions (subst
                           ,occurs
                           ,concretize
                           ,rescheme
                           ,generalize) where

import Context
import Control.DeepSeq (NFData (..), deepseq)
import Control.Monad (liftM)
import Data.List (foldl')
import qualified Data.Set as Set
import qualified Data.Map as Map
import Guid
import Types.Types

class Subst a where
  subst :: [(X,Type)] -> a -> a

instance Subst Type where
 subst ss t =
  case t of
   VarT x -> case lookup x ss of
               Nothing        -> VarT x
               Just (Super _) -> VarT x
               Just t         -> t
   LambdaT t1 t2 -> LambdaT (subst ss t1) (subst ss t2)
   ADT name ts -> ADT name (subst ss ts)
   RecordT fs t -> RecordT (Map.map (subst ss) fs) (subst ss t)
   EmptyRecord -> EmptyRecord
   Super ts -> Super ts

instance Subst Scheme where
  subst ss (Forall vs cs t) = Forall vs (subst ss cs) (subst ss t)

instance Subst Constraint where
  subst ss (t1 :=: t2) = subst ss t1 :=: subst ss t2
  subst ss (t :<: super) = subst ss t :<: super
  subst ss (x :<<: poly) = x :<<: subst ss poly

instance Subst a => Subst (Context a) where
  subst ss (C str span c) = C str span (subst ss c)

instance Subst a => Subst [a] where
  subst ss as = map (subst ss) as


class FreeVars a where
  freeVars :: a -> [X]

instance FreeVars Type where
  freeVars (VarT v) = [v]
  freeVars (LambdaT t1 t2) = freeVars t1 ++ freeVars t2
  freeVars (ADT _ ts) = concatMap freeVars ts
  freeVars (RecordT fs t) =
      freeVars (concat $ Map.elems fs) ++ freeVars t
  freeVars EmptyRecord = []
  freeVars (Super _ ) = [] 

instance FreeVars Constraint where
  freeVars (t1 :=: t2) = freeVars t1 ++ freeVars t2
  freeVars (t1 :<: t2) = freeVars t1 ++ freeVars t2
  freeVars (x :<<: Forall xs cs t) = filter (`notElem` xs) frees
      where frees = concatMap freeVars cs ++ freeVars t

instance FreeVars a => FreeVars (Context a) where
  freeVars (C _ _ c) = freeVars c    

instance FreeVars a => FreeVars [a] where
  freeVars = concatMap freeVars

occurs x t = x `elem` freeVars t

concretize :: Scheme -> GuidCounter (Type, [Context Constraint])
concretize (Forall xs cs t) = do
  ss <- zip xs `liftM` mapM (\_ -> liftM VarT guid) xs
  return (subst ss t, subst ss cs)

rescheme :: Scheme -> GuidCounter Scheme
rescheme (Forall xs cs t) = do
  xs' <- mapM (const guid) xs
  let ss = zip xs (map VarT xs')
  return $ Forall xs' (subst ss cs) (subst ss t)

generalize :: [X] -> Scheme -> GuidCounter Scheme
generalize exceptions (Forall xs cs t) = rescheme (Forall (xs ++ frees) cs t)
    where allFrees = Set.fromList $ freeVars t ++ concatMap freeVars cs
          frees = Set.toList $ Set.difference allFrees (Set.fromList exceptions)
