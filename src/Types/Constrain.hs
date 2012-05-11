{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Constrain where

import Ast
import Types
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Monad (liftM,mapM)
import Control.Monad.State (evalState, State, get, put)

data Constraint = Type :=: Type
                | Type :<: Type
                | Type :<<: Scheme
                  deriving (Eq, Ord, Show)

-- Wrapper around State monad.
newtype GuidCounter a = GC { runGC :: State Int a }
    deriving (Monad)

-- Get the next GUID, incrementing the counter.
guid :: GuidCounter Int
guid = GC $ do n <- get
               put (n + 1)
               return n

beta = VarT `liftM` guid
unionA = Map.unionWith (++)
unionsA = Map.unionsWith (++)

constrain expr = Set.toList cs
    where (as,cs,t) = evalState (runGC $ inference expr) 0

inference (Var x) =
    do b <- beta
       return (Map.singleton x [b], Set.empty, b)
inference (App e1 e2) =
    do (a1,c1,t1) <- inference e1
       (a2,c2,t2) <- inference e2
       b <- beta
       return ( unionA a1 a2
              , Set.unions [c1,c2,Set.singleton $ t1 :=: (LambdaT t2 b)]
              , b )
inference (Lambda x e) =
    do (a,c,t) <- inference e
       b <- beta
       return ( Map.delete x a
              , Set.union c . Set.fromList . map (:=: b) $
                          Map.findWithDefault [] x a
              , LambdaT b t )
inference (Let defs e) =
    do (a,c,t) <- inference e
       let (xs,es) = unzip defs
       (as,cs,ts) <- unzip3 `liftM` mapM inference es
       let assumptions = unionsA (a:as)
       let f x t = map (:<: t) $ Map.findWithDefault [] x assumptions
       let constraints = Set.fromList . concat $ zipWith f xs ts
       return ( foldr Map.delete assumptions xs
              , Set.unions $ c:constraints:cs
              , t )

inference (If e1 e2 e3) =
    do (a1,c1,t1) <- inference e1
       (a2,c2,t2) <- inference e2
       (a3,c3,t3) <- inference e3
       return ( unionsA [a1,a2,a3]
              , Set.unions [c1,c2,c3, Set.fromList [ t1 :=: BoolT, t2 :=: t3 ] ]
              , t2 )

inference (Data name es) =
    do (as,cs,ts) <- unzip3 `liftM` mapM inference es
       return ( unionsA as, Set.unions cs, ADT name ts )

inference other =
    case other of
      Number _ -> primitive IntT
      Chr _ -> primitive CharT
      Boolean _ -> primitive BoolT
      _ -> beta >>= primitive 

primitive t = return (Map.empty, Set.empty, t)
{--
      Let x e1 e2 ->
          do (a1,c1,t1) <- inference e1
             (a2,c2,t2) <- inference e2
             let ts = Map.findWithDefault (error "inference") x a
             return ( unionA a1 $ Map.delete x a2
                    , Set.unions [ c1, c2, Set.fromList $ map (:<: t1) ts ]
                    , t2 )
--}
