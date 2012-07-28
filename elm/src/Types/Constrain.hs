
module Constrain where

import Ast
import Types
import Data.List (foldl')
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Monad (liftM,mapM,zipWithM)
import Control.Monad.State (evalState)
import Guid

beta = VarT `liftM` guid
unionA = Map.unionWith (++)
unionsA = Map.unionsWith (++)

data Context a b = Context a b deriving (Eq,Show)
ctx e = Context e

instance (Ord a, Eq c) => Ord (Context c a) where
    compare (Context _ x) (Context _ y) = compare x y

constrain hints expr = do
    (as,cs,t) <- inference expr
    let cMap = Map.intersectionWithKey combine (Map.fromList hints) as
            where combine k s vs = map (\v -> ctx (Var k) $ VarT v :<<: s) vs
    return (Set.toList cs ++ (concatMap snd $ Map.toList cMap))

inference :: Expr -> GuidCounter (Map.Map String [X], Set.Set (Context Expr Constraint), Type)
inference (Var x) =
    do b <- guid
       return (Map.singleton x [b], Set.empty, VarT b)
inference e@(App e1 e2) =
    do (a1,c1,t1) <- inference e1
       (a2,c2,t2) <- inference e2
       b <- beta
       return ( unionA a1 a2
              , Set.unions [c1,c2,Set.singleton . ctx e $ t1 :=: (LambdaT t2 b)]
              , b )
inference (Lambda x e) =
    do (a,c,t) <- inference e
       b <- beta
       v <- guid
       return ( Map.delete x a
              , Set.union c . Set.fromList . map (\x -> ctx e $ VarT x :=: b) $
                          Map.findWithDefault [v] x a
              , LambdaT b t )
inference (Let defs e) =
    do (as,cs,t) <- inference e
       (ass, schemes) <- liftM unzip (mapM defScheme defs)
       let assumptions = unionsA (as:ass)
       let names = map (\(Definition x _ _) -> x) defs
       let genCs name s = do
             v <- guid
             return . map (\x -> ctx (Var name) $ VarT x :<<: s) $ Map.findWithDefault [v] name assumptions
       cs' <- zipWithM genCs names schemes
       return ( foldr Map.delete assumptions names
              , Set.union (Set.fromList . concat $ cs') cs
              , t )

inference ce@(Case e cases) =
    do (as,cs,t) <- inference e
       (ass,css,ts) <- liftM unzip3 $ mapM (caseInference t) cases
       return ( unionsA $ as:ass
              , let combine t1 t2 = ctx ce (t1 :=: t2) in
                Set.unions $ Set.fromList (zipWith combine ts $ tail ts) : cs : css
              , head ts)

inference e@(If e1 e2 e3) =
    do (a1,c1,t1) <- inference e1
       (a2,c2,t2) <- inference e2
       (a3,c3,t3) <- inference e3
       return ( unionsA [a1,a2,a3]
              , Set.unions [c1,c2,c3, Set.fromList [ ctx e1 (t1 :=: bool), ctx e (t2 :=: t3) ] ]
              , t2 )

inference (Data name es) = inference $ foldl' App (Var name) es
inference (Binop op e1 e2) = inference (Var op `App` e1 `App` e2)
inference (Access (Var x) y) = inference . Var $ x ++ "." ++ y
inference (Range e1 e2) = inference (Var "elmRange" `App` e1 `App` e2)

inference other =
    case other of
      IntNum _ -> do t <- beta
                     return (Map.empty, Set.singleton (ctx other $ t :<: number), t)
      FloatNum _ -> primitive float
      Chr _ -> primitive char
      Str _ -> primitive string
      Boolean _ -> primitive bool
      _ -> beta >>= primitive 

primitive t = return (Map.empty, Set.empty, t)

caseInference tipe (p,e) = do
  (as,cs,t) <- inference e
  return ( foldr Map.delete as (namesIn p)
         , cs
         , t)

namesIn PAnything = []
namesIn (PVar v)  = [v]
namesIn (PData name ps) = concatMap namesIn ps

defScheme (Definition name args e) =
    do argDict <- mapM (\a -> liftM ((,) a) guid) args 
       (as,cs,t) <- inference e
       let genCs (arg,x) = do
             v <- guid
             return . map (\y -> VarT x :=: VarT y) $ Map.findWithDefault [v] arg as
       cs' <- concat `liftM` mapM genCs argDict
       let cs'' = map (\(Context _ c) -> c) (Set.toList cs)
       let tipe = foldr (==>) t $ map (VarT . snd) argDict
       return ( foldr Map.delete as args
              , Forall (map snd argDict) (cs' ++ cs'') tipe)
