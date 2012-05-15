
module Constrain where

import Ast
import Types
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Monad (liftM,mapM)
import Control.Monad.State (evalState)
import Guid
import Hints

data Constraint = Type :=: Type
                | Type :<: Type
                | Type :<<: Scheme
                  deriving (Eq, Ord, Show)

beta = VarT `liftM` guid
unionA = Map.unionWith (++)
unionsA = Map.unionsWith (++)

constrain expr = run $ do
    (as,cs,t) <- inference expr
    hs <- hints
    let cMap = Map.intersectionWith (\t -> map (:<: t)) (Map.fromList hs) as
    return $ Set.toList cs ++ (concat . map snd $ Map.toList cMap)

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

inference (Data name es) = inference $ foldl App (Var name) es
inference (Binop op e1 e2) = inference (Var op `App` e1 `App` e2)
inference (Access (Var x) y) = inference . Var $ x ++ "." ++ y
inference (Range e1 e2) = inference (Var "elmRange" `App` e1 `App` e2)

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
