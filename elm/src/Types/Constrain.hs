
module Types.Constrain (constrain) where

import Ast
import Types
import Data.List (foldl')
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Arrow (second)
import Control.Monad (liftM,mapM,zipWithM,foldM)
import Control.Monad.State (evalState)
import Guid
import Types.Substitutions

--import System.IO.Unsafe

prints xs v = v --} unsafePerformIO (putStrLn "~~~~~~~~~~" >> mapM print xs) `seq` v

beta = VarT `liftM` guid
unionA = Map.unionWith (++)
unionsA = Map.unionsWith (++)

constrain hints (Module _ _ _ stmts) = do
    (ass,css,schemess) <- unzip3 `liftM` mapM stmtGen stmts
    allHints <-  liftM (Map.fromList . (++ concat schemess)) hints
    let insert as n = do v <- guid; return $ Map.insertWith' (\_ x -> x) n [v] as
    assumptions <- foldM insert (unionsA ass) $ map fst (concat schemess)
    let cs = let f k s vs = map (\v -> Context k $ v :<<: s) vs in
             concat . Map.elems $ Map.intersectionWithKey f allHints assumptions
    let escapees = Map.keys $ Map.difference assumptions allHints
    return . (,) escapees $ cs ++ Set.toList (Set.unions css)

gen :: Expr -> GuidCounter (Map.Map String [X], Set.Set (Context String Constraint), Type)

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
             let vs = Map.findWithDefault [v] name assumptions
             return $ map (\x -> ctx (Var name) $ x :<<: s) vs
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
gen (Range e1 e2) =
    do (a1,c1,t1) <- gen e1
       (a2,c2,t2) <- gen e2
       return ( unionsA [a1,a2]
              , Set.unions [ c1, c2, Set.fromList [ ctx e1 (t1 :=: int)
                                                  , ctx e2 (t2 :=: int) ] ]
              , listOf int )

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
  (as ,cs , t  ) <- gen e
  (as',cs',[t']) <- patternGen (as,cs,[]) p
  let cs'' = Set.union cs' . Set.singleton . ctx p $ t' :=: tipe
  return ( as', cs'', t )

patternGen (as,cs,ts) PAnything = ((,,) as cs . (\t -> ts++[t])) `liftM` beta
patternGen (as,cs,ts) (PVar v) = do
  b <- beta
  let cs' = map (\x -> ctx (Var v) $ VarT x :=: b) $ Map.findWithDefault [] v as
  return ( Map.delete v as, Set.union cs $ Set.fromList cs', ts ++ [b] )
patternGen (as,cs,ts) p@(PData name ps) = do
  constr <- guid
  output <- beta
  (as',cs',ts') <- foldM patternGen (as,cs,[]) ps
  let t = foldr (==>) output ts'
  let getC | isTupleString name = do
        vs <- mapM (\_ -> beta) ps
        return . Set.singleton . ctx p $ output :=: ADT name vs
           | otherwise = return Set.empty
  cs'' <- getC
  return ( unionA as' (Map.singleton name [constr])
         , Set.unions [cs',cs'', Set.singleton . ctx p $ VarT constr :=: t]
         , ts ++ [output] )


defScheme :: Definition -> GuidCounter (Map.Map String [X], Scheme)
defScheme (Definition name args e) = do
  (as,cs,hint) <- defGen name args e
  return ( as, snd hint )

defGen name args e = do
  argDict <- mapM (\a -> liftM ((,) a) guid) args 
  (as,cs,t) <- gen e
  let genCs (arg,x) = do
        v <- guid
        return . map (\y -> Context arg $ VarT x :=: VarT y) $ Map.findWithDefault [v] arg as
  cs' <- concat `liftM` mapM genCs argDict
  let as' = foldr Map.delete as args
  let tipe = foldr (==>) t $ map (VarT . snd) argDict
  scheme <- generalize (concat $ Map.elems as') $
            Forall (map snd argDict) (cs' ++ Set.toList cs) tipe
  return ( as', Set.empty, (name, scheme) )

stmtGen (Def name args e) = do (as,cs,hint) <- defGen name args e
                               return ( as, cs, [hint] )

stmtGen (Datatype name xs tcs) = do schemes <- mapM gen' tcs'
                                    return (Map.empty, Set.empty, schemes)
    where names = map (+ (length xs)) [1..5]
          tcs' = map (second . map $ rnm names) tcs
          supers t = map (Context name) $ zipWith (:<:) (map VarT names)
                     [ number, time, appendable t, comparable, transformable ]
          gen' (n,ts) = do t <- beta
                           let s = Forall (xs ++ names) (supers t) $
                                   foldr (==>) (ADT name $ map VarT xs) ts
                           (,) n `liftM` generalize [] s
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
         , Set.insert (ctx base (signalOf t :=: tipe)) cs
         , [ (elm, Forall [] [] tipe) ] )

getDatatypeInfo (Datatype name args tcs) =
    Just (name, args, tcs)
getDatatypeInfo _ = Nothing

