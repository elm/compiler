
module Constrain (constrain, Context (..)) where

import Ast
import Types
import Data.List (foldl')
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Arrow (second)
import Control.Monad (liftM,mapM,zipWithM)
import Control.Monad.State (evalState)
import Guid

beta = VarT `liftM` guid
unionA = Map.unionWith (++)
unionsA = Map.unionsWith (++)

data Context a b = Context a b deriving (Eq, Show)
ctx e = Context e

instance (Ord a, Eq c) => Ord (Context c a) where
    compare (Context _ x) (Context _ y) = compare x y

constrain hints (Module _ _ _ stmts) = do
    (ass,css,schemess) <- unzip3 `liftM` mapM stmtGen stmts
    let allHints = Map.fromList $ hints ++ concat schemess
    let combine k s vs = map (\v -> ctx (Var k) $ VarT v :<<: s) vs
    let cMap = Map.intersectionWithKey combine allHints (unionsA ass)
    return (Set.toList (Set.unions css) ++ (concatMap snd $ Map.toList cMap))

gen :: Expr -> GuidCounter (Map.Map String [X], Set.Set (Context Expr Constraint), Type)

gen (Var x) =
    do b <- guid
       return (Map.singleton x [b], Set.empty, VarT b)

gen e@(App e1 e2) =
    do (a1,c1,t1) <- gen e1
       (a2,c2,t2) <- gen e2
       b <- beta
       return ( unionA a1 a2
              , Set.unions [c1,c2,Set.singleton . ctx e $ t1 :=: (LambdaT t2 b)]
              , b )
gen (Lambda x e) =
    do (a,c,t) <- gen e
       b <- beta
       v <- guid
       return ( Map.delete x a
              , Set.union c . Set.fromList . map (\x -> ctx e $ VarT x :=: b) $
                          Map.findWithDefault [v] x a
              , LambdaT b t )
gen (Let defs e) =
    do (as,cs,t) <- gen e
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

gen ce@(Case e cases) =
    do (as,cs,t) <- gen e
       (ass,css,ts) <- liftM unzip3 $ mapM (caseGen t) cases
       return ( unionsA $ as:ass
              , let combine t1 t2 = ctx ce (t1 :=: t2) in
                Set.unions $ Set.fromList (zipWith combine ts $ tail ts) : cs : css
              , head ts)

gen e@(If e1 e2 e3) =
    do (a1,c1,t1) <- gen e1
       (a2,c2,t2) <- gen e2
       (a3,c3,t3) <- gen e3
       return ( unionsA [a1,a2,a3]
              , Set.unions [c1,c2,c3, Set.fromList [ ctx e1 (t1 :=: bool), ctx e (t2 :=: t3) ] ]
              , t2 )

gen (Data name es) = gen $ foldl' App (Var name) es
gen (Binop op e1 e2) = gen (Var op `App` e1 `App` e2)
gen (Access (Var x) y) = gen . Var $ x ++ "." ++ y
gen (Range e1 e2) = gen (Var "elmRange" `App` e1 `App` e2)

gen other =
    case other of
      IntNum _ -> do t <- beta
                     return (Map.empty, Set.singleton (ctx other $ t :<: number), t)
      FloatNum _ -> primitive float
      Chr _ -> primitive char
      Str _ -> primitive string
      Boolean _ -> primitive bool
      _ -> beta >>= primitive 

primitive t = return (Map.empty, Set.empty, t)

caseGen tipe (p,e) = do
  (as,cs,t) <- gen e
  return ( foldr Map.delete as (namesIn p)
         , cs
         , t)

namesIn PAnything = []
namesIn (PVar v)  = [v]
namesIn (PData name ps) = concatMap namesIn ps

defScheme (Definition name args e) =
    do argDict <- mapM (\a -> liftM ((,) a) guid) args 
       (as,cs,t) <- gen e
       let genCs (arg,x) = do
             v <- guid
             return . map (\y -> VarT x :=: VarT y) $ Map.findWithDefault [v] arg as
       cs' <- concat `liftM` mapM genCs argDict
       let cs'' = map (\(Context _ c) -> c) (Set.toList cs)
       let tipe = foldr (==>) t $ map (VarT . snd) argDict
       return ( foldr Map.delete as args
              , Forall (map snd argDict) (cs' ++ cs'') tipe)

stmtGen (Def name args e) = do
  argDict <- mapM (\a -> liftM ((,) a) guid) args 
  (as,cs,t) <- gen e
  let genCs (arg,x) = do
        v <- guid
        return . map (\y -> VarT x :=: VarT y) $ Map.findWithDefault [v] arg as
  cs' <- concat `liftM` mapM genCs argDict
  let cs'' = map (\(Context _ c) -> c) (Set.toList cs)
  let tipe = foldr (==>) t $ map (VarT . snd) argDict
  return ( foldr Map.delete as args
         , Set.empty
         , [ (name, Forall (map snd argDict) (cs' ++ cs'') tipe) ] )

stmtGen (Datatype name xs tcs) = do schemes <- mapM gen' tcs'
                                    return (Map.empty, Set.empty, schemes)
    where names = map (+ (length xs)) [1..5]
          tcs' = map (second . map $ rnm names) tcs
          supers t = zipWith (:<:) (map VarT names)
                     [ number, time, appendable t, comparable, transformable ]
          gen' (n,ts) = do
            t <- beta
            return . (,) n . Forall (xs ++ names) (supers t) $
                   foldr (==>) (ADT name $ map VarT xs) ts
          rnm [a,b,c,d,e] (ADT n []) | n == "Number" = VarT a
                                     | n == "Time"  = VarT b
                                     | n == "Appendable"  = VarT c
                                     | n == "Comparable"  = VarT d
                                     | n == "Transformable"  = VarT e
          rnm _ t = t

stmtGen (ExportEvent js elm tipe) = do
  x <- guid
  return ( Map.singleton elm [x]
         , Set.singleton . ctx (Var elm) $ VarT x :=: tipe
         , [] )

stmtGen (ImportEvent js base elm tipe) = do
  (as,cs,t) <- gen base
  return ( as
         , Set.insert (ctx (Var elm) (t :=: tipe)) cs
         , [ (elm, Forall [] [] tipe) ] )

ffiHints (ims,exs) =
    map (\(_,_,n,t) -> n -: t) ims ++ map (\(_,n,t) -> n -: t) exs

checkFFI dict ims exs = list $ ims' ++ exs'
  where ims' = map (\(_,b,n,_)-> Binop "==" (Var n) (App (Var "constant") b)) ims
        exs' = map (\(_,n,_)  -> Binop "==" (Var n) (Var $ dict n)) exs