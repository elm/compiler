
module Constrain where

import Ast
import Types
import Data.List (foldl')
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Monad (liftM,mapM)
import Control.Monad.State (evalState)
import Guid

beta = VarT `liftM` guid
unionA = Map.unionWith (++)
unionsA = Map.unionsWith (++)

constrain hints expr = do
    (as,cs,t) <- inference expr
    let cMap = Map.intersectionWith (\s -> map (\v -> VarT v:<<: s)) (Map.fromList hints) as
    return $ Set.toList cs ++ (concat . map snd $ Map.toList cMap)

inference :: Expr -> GuidCounter (Map.Map String [X], Set.Set Constraint, Type)
inference (Var x) =
    do b <- guid
       return (Map.singleton x [b], Set.empty, VarT b)
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
              , Set.union c . Set.fromList . map (\x -> VarT x :=: b) $
                          Map.findWithDefault [] x a
              , LambdaT b t )
inference (Let defs e) =
    do (a,c,t) <- inference e
       let (xs,es) = unzip defs
       (as,cs,ts) <- unzip3 `liftM` mapM inference es
       let assumptions = unionsA (a:as)
       let f x t = map (:=: t) . map VarT $ Map.findWithDefault [] x assumptions
       let constraints = Set.fromList . concat $ zipWith f xs ts
       return ( foldr Map.delete assumptions xs
              , Set.unions $ c:constraints:cs
              , t )

inference (If e1 e2 e3) =
    do (a1,c1,t1) <- inference e1
       (a2,c2,t2) <- inference e2
       (a3,c3,t3) <- inference e3
       return ( unionsA [a1,a2,a3]
              , Set.unions [c1,c2,c3, Set.fromList [ t1 :=: bool, t2 :=: t3 ] ]
              , t2 )

inference (Data name es) = inference $ foldl' App (Var name) es
inference (Binop op e1 e2) = inference (Var op `App` e1 `App` e2)
inference (Access (Var x) y) = inference . Var $ x ++ "." ++ y
inference (Range e1 e2) = inference (Var "elmRange" `App` e1 `App` e2)

inference other =
    case other of
      IntNum _ -> do t <- beta
                     return (Map.empty, Set.singleton (t :<: number), t)
      FloatNum _ -> primitive float
      Chr _ -> primitive char
      Str _ -> primitive string
      Boolean _ -> primitive bool
      _ -> beta >>= primitive 

primitive t = return (Map.empty, Set.empty, t)
