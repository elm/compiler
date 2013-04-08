{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Types.Substitutions (subst,
                            occurs,
                            freeVars,
                            concretize,
                            rescheme,
                            generalize,
                            superize) where

import Ast
import Context
import Control.Monad (liftM, liftM2)
import Control.Monad.State (runState, State, get, put)
import Data.List (foldl')
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Guid
import Types.Types as Types

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

newtype Superize a = S { runSuper :: State (Int, Maybe X, Maybe X) a }
    deriving (Monad)

superize :: String -> Type -> GuidCounter Scheme
superize name tipe =
    do x <- guid
       let (tipe', (m, number, append)) =
               runState (runSuper (go tipe)) (x+1, Nothing, Nothing)
           mkC super = liftM (\x -> C (Just name) NoSpan (VarT x :<: super))
       set m
       return (Forall (catMaybes [number,append])
                      (catMaybes [ mkC Types.number number
                                 , mkC (Types.appendable x) append])
                      tipe')
 where
  go :: Type -> Superize Type
  go t =
    case t of
      EmptyRecord -> return t
      Super _     -> return t
      VarT _      -> return t
      LambdaT t1 t2       -> liftM2 LambdaT (go t1) (go t2)
      ADT "Number" []     -> liftM VarT number
      ADT "Appendable" [] -> liftM VarT append
      ADT name ts         -> liftM (ADT name) (mapM go ts)
      RecordT fs t        -> liftM2 RecordT fs' (go t)
          where pairs = Map.toList fs
                fs' = do ps <- mapM (\(f,t) -> liftM ((,) f) (mapM go t)) pairs
                         return (Map.fromList ps)

  number :: Superize X
  number = S $ do (n, num, append) <- get
                  case num of
                    Just x  -> return x
                    Nothing -> do put (n + 1, Just n, append)
                                  return n

  append :: Superize X
  append = S $ do (n, number, app) <- get
                  case app of
                    Just x  -> return x
                    Nothing -> do put (n + 1, number, Just n)
                                  return n