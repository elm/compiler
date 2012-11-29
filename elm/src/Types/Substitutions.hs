
module Types.Substitutions (subst
                           ,occurs
                           ,concretize
                           ,rescheme
                           ,generalize) where

import Control.DeepSeq (NFData (..), deepseq)
import Control.Monad (liftM)
import Data.List (foldl')
import qualified Data.Set as Set
import Guid
import Types.Types


class Subst a where
  subst :: [(X,Type)] -> a -> a

instance Subst Type where
  subst ss (VarT x) = case lookup x ss of
                        Nothing        -> VarT x
                        Just (Super _) -> VarT x
                        Just t         -> t
  subst ss (LambdaT t1 t2) = LambdaT (subst ss t1) (subst ss t2)
  subst ss (ADT name ts) = ADT name (subst ss ts)
  subst ss (Super ts) = Super ts

instance Subst Scheme where
  subst ss (Forall vs cs t) = Forall vs (subst ss cs) (subst ss t)

instance Subst Constraint where
  subst ss (t1 :=: t2) = subst ss t1 :=: subst ss t2
  subst ss (t :<: super) = subst ss t :<: super
  subst ss (x :<<: poly) = x :<<: subst ss poly

instance Subst a => Subst (Context c a) where
  subst ss (Context ctx c) = Context ctx (subst ss c)

instance Subst a => Subst [a] where
  subst ss as = map (subst ss) as


class FreeVars a where
  freeVars :: a -> [X]

instance FreeVars Type where
  freeVars (VarT v) = [v]
  freeVars (LambdaT t1 t2) = freeVars t1 ++ freeVars t2
  freeVars (ADT _ ts) = concatMap freeVars ts
  freeVars (Super _ ) = [] 

instance FreeVars Constraint where
  freeVars (t1 :=: t2) = freeVars t1 ++ freeVars t2
  freeVars (t1 :<: t2) = freeVars t1 ++ freeVars t2
  freeVars (x :<<: Forall xs cs t) = filter (`notElem` xs) frees
      where frees = concatMap freeVars cs ++ freeVars t

instance FreeVars a => FreeVars (Context c a) where
  freeVars (Context _ c) = freeVars c    


class Occurs a where
  occurs :: X -> a -> Bool

instance Occurs Type where
  occurs v (VarT x) = v == x
  occurs v (LambdaT t1 t2) = occurs v t1 || occurs v t2
  occurs v (ADT _ ts) = any (occurs v) ts
  occurs v (Super _ ) = False

instance Occurs Constraint where
  occurs x (t1 :=: t2) = occurs x t1 || occurs x t2
  occurs x (t1 :<: t2) = occurs x t1 || occurs x t2
  occurs x (y :<<: Forall xs cs t) = x == y || occurs x t || inCs
      where inCs = x `notElem` xs && any (occurs x) cs

instance Occurs a => Occurs (Context c a) where
  occurs x (Context _ c) = occurs x c

concretize :: Scheme -> GuidCounter (Type, [Context String Constraint])
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
