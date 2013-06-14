{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Types.Substitutions (subst,
                            occurs,
                            freeVars,
                            concretize,
                            rescheme,
                            generalize,
                            superize) where

import SourceSyntax.Everything 
import Control.Monad (liftM, liftM2)
import Control.Monad.State (runState, State, get, put)
import Data.List (nub)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Unique
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
  subst ss (x :<<: poly) = (case lookup x ss of
                               Just (VarT y) -> y
                               _ -> x
                           ) :<<: subst ss poly

instance Subst a => Subst (Located a) where
  subst ss (L str span c) = L str span (subst ss c)

instance Subst a => Subst [a] where
  subst ss as = map (subst ss) as


class FreeVars a where
  freeVars :: a -> [X]

instance FreeVars Type where
  freeVars t =
    case t of
      VarT v        -> [v]
      LambdaT t1 t2 -> freeVars t1 ++ freeVars t2
      ADT _ ts      -> concatMap freeVars ts
      RecordT fs t  -> freeVars (concat $ Map.elems fs) ++ freeVars t
      EmptyRecord   -> []
      Super _       -> []

instance FreeVars Constraint where
  freeVars (t1 :=: t2) = freeVars t1 ++ freeVars t2
  freeVars (t1 :<: t2) = freeVars t1 ++ freeVars t2
  freeVars (x :<<: Forall xs cs t) = filter (`notElem` xs) frees
      where frees = concatMap freeVars cs ++ freeVars t

instance FreeVars a => FreeVars (Located a) where
  freeVars (L _ _ c) = freeVars c    

instance FreeVars a => FreeVars [a] where
  freeVars = concatMap freeVars

occurs x t = x `elem` freeVars t

concretize :: Scheme -> Unique (Type, [Located Constraint])
concretize (Forall xs cs t) = do
  ss <- zip xs `liftM` mapM (\_ -> liftM VarT guid) xs
  return (subst ss t, subst ss cs)

rescheme :: Scheme -> Unique Scheme
rescheme (Forall xs cs t) = do
  xs' <- mapM (const guid) xs
  let ss = zip xs (map VarT xs')
  return $ Forall xs' (subst ss cs) (subst ss t)

generalize :: [X] -> Scheme -> Unique Scheme
generalize exceptions (Forall xs cs t) = rescheme (Forall (xs ++ frees) cs t)
    where allFrees = Set.fromList $ freeVars t ++ concatMap freeVars cs
          frees = Set.toList $ Set.difference allFrees (Set.fromList exceptions)

newtype Superize a = S { runSuper :: State ([X], [X], [X]) a }
    deriving (Monad)

superize :: String -> Type -> Unique Scheme
superize name tipe =
    do constraints <- liftM concat $
                      sequence [ mapM (<: nmbr) (nub ns)
                               , mapM (<: apnd) (nub as)
                               , mapM (<: comp) (nub cs) ]
       return (Forall (concat [ns,as,cs]) constraints tipe')
 where
  (tipe', (ns,as,cs)) = runState (runSuper (go tipe)) ([],[],[])
  t <: super = do x <- guid
                  return $ L (Just name) NoSpan (VarT t :<: super x)

  nmbr t = number
  apnd t = appendable t
  comp t = comparable t

  go :: Type -> Superize Type
  go t =
    case t of
      EmptyRecord -> return t
      Super _     -> return t
      VarT _      -> return t
      LambdaT t1 t2        -> liftM2 LambdaT (go t1) (go t2)
      ADT "Number" [VarT t] -> addNumber t
      ADT "Appendable" [VarT t] -> addAppendable t
      ADT "Comparable" [VarT t] -> addComparable t
      ADT name ts          -> liftM (ADT name) (mapM go ts)
      RecordT fs t         -> liftM2 RecordT fs' (go t)
          where pairs = Map.toList fs
                fs' = do ps <- mapM (\(f,t) -> liftM ((,) f) (mapM go t)) pairs
                         return (Map.fromList ps)

  add :: (X -> ([X],[X],[X]) -> ([X],[X],[X])) -> X -> Superize Type
  add f v = S $ do (ns, as, cs) <- get
                   put $ f v (ns, as, cs)
                   return (VarT v)

  addNumber     = add (\n (ns,as,cs) -> (n:ns,as,cs))
  addAppendable = add (\a (ns,as,cs) -> (ns,a:as,cs))
  addComparable = add (\c (ns,as,cs) -> (ns,as,c:cs))