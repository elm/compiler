
module Types.Constrain (constrain) where

import Control.Arrow (second)
import Control.Monad (liftM,mapM,zipWithM,foldM)
import Control.Monad.State (evalState)
import Data.Char (isDigit)
import Data.List (foldl',sort,group,isPrefixOf,intercalate,isSuffixOf)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Ast
import Context
import Guid
import qualified Libraries as Libraries

import Types.Types
import Types.Substitutions

beta = VarT `liftM` guid
unionA = Map.unionWith (++)
unionsA = Map.unionsWith (++)

getAliases imports hints = hints ++ concatMap aliasesFrom imports
  where aliasesFrom (name,method) =
            let values = concatMap (getValue name) hints
            in  case method of
                  As alias -> if alias == name then [] else
                                  map (\(n,t) -> (alias ++ "." ++ n, t)) values
                  Hiding vs -> filter (\(n,t) -> n `notElem` vs) values
                  Importing vs -> filter (\(n,t) -> n `elem` vs) values
        getValue inModule (name,tipe) =
            case inModule `isPrefixOf` name of
              True  -> [ (drop (length inModule + 1) name, tipe) ]
              False -> []

findAmbiguous hints hints' assumptions continue =
  let potentialDups = map head . filter (\g -> length g > 1) . group $ sort hints'
      dups = filter (\k -> Map.member k assumptions) potentialDups
  in case dups of
       n:_ -> return . Left $ "Error: Ambiguous occurrence of '" ++ n ++
              "' could refer to " ++ intercalate ", " (filter (isSuffixOf n) hints)
       _ -> continue

mergeSchemes :: [Map.Map String Scheme]
             -> GuidCounter (TVarMap, ConstraintSet, Map.Map String Scheme)
mergeSchemes schmss = do (ass,css,sss) <- unzip3 `liftM` mapM split kvs
                         return (Map.unions ass, Set.unions css, Map.unions sss)
  where
    kvs = Map.toList $ Map.unionsWith (++) (map (Map.map (:[])) schmss)
    split (k,vs) =
      let ps = zipWith (\s v -> (s++k,v)) (map (flip replicate '_') [0..]) vs
          eq t u = C (Just $ msg ++ k) NoSpan (VarT t :=: VarT u)
          msg = "the definition of "
      in  do xs <- mapM (\_ -> guid) vs
             return ( Map.fromList $ zip (map fst ps) (map (:[]) xs)
                    , case xs of
                        t:ts -> Set.fromList $ zipWith eq (t:ts) ts
                        []   -> Set.empty
                    , Map.fromList ps )

constrain typeHints (Module _ _ imports stmts) = do
  (ass,css,schemess) <- unzip3 `liftM` mapM stmtGen stmts
  hints <- typeHints
  (as', cs', schemes) <- mergeSchemes schemess
  let constraints = Set.unions (cs':css)
      as = unionsA (as':ass)
      aliasHints = getAliases (imports ++ Libraries.prelude) hints
      allHints = Map.union schemes (Map.fromList aliasHints)
      insert as n = do v <- guid; return $ Map.insertWith' (\_ x -> x) n [v] as
  assumptions <- foldM insert as (Map.keys schemes)
  findAmbiguous (map fst hints) (map fst aliasHints) assumptions $ do
    let f k s vs = map (\v -> C (Just k) NoSpan $ v :<<: s) vs
        cs = concat . Map.elems $ Map.intersectionWithKey f allHints assumptions
        escapees = Map.keys $ Map.difference assumptions allHints
    return $ case escapees of
               _  -> Right (Set.toList constraints ++ cs)
               --_  -> Left ("Undefined variable(s): " ++ intercalate ", " escapees)

type TVarMap = Map.Map String [X]
type ConstraintSet = Set.Set (Context Constraint)

ctx e span = C (Just $ show e) span

gen :: CExpr -> GuidCounter (TVarMap, ConstraintSet, Type)
gen (C _ span expr) =
  let ctx' = C (Just $ show expr) span in
  case expr of
    Var x ->
        do b <- guid
           return (Map.singleton x [b], Set.empty, VarT b)

    App e1 e2 ->
        do (a1,c1,t1) <- gen e1
           (a2,c2,t2) <- gen e2
           b <- beta
           return ( unionA a1 a2
                  , Set.unions [c1,c2
                               ,Set.singleton . ctx' $ t1 :=: (LambdaT t2 b)]
                  , b )

    Lambda x e ->
        do (a,c,t) <- gen e
           b <- beta
           v <- guid
           return ( Map.delete x a
                  , Set.union c . Set.fromList .
                    map (\x -> ctx' $ VarT x :=: b) $
                    Map.findWithDefault [v] x a
                  , LambdaT b t )

    Let defs e ->
        do (as,cs,t) <- gen e
           (ass, schemes) <- liftM unzip (mapM defScheme defs)
           let assumptions = unionsA (as:ass)
               getName d = case d of FnDef f _ _    -> f
                                     OpDef op _ _ _ -> op
               names = map getName defs
               genCs name s = do
                 v <- guid
                 let vs = Map.findWithDefault [v] name assumptions
                 return $ map (\x -> ctx name span $ x :<<: s) vs
           cs' <- zipWithM genCs names schemes
           return ( foldr Map.delete assumptions names
                  , Set.union (Set.fromList . concat $ cs') cs
                  , t )

    Case e cases ->
        do (as,cs,t) <- gen e
           (ass,css,ts) <- liftM unzip3 $ mapM (caseGen t) cases
           return ( unionsA $ as:ass
                  , let cases' = map snd cases
                        ctxs = zipWith epos cases' (tail cases')
                        csts = zipWith (:=:) ts (tail ts)
                        cs' = Set.fromList (zipWith ($) ctxs csts)
                    in  Set.unions $ cs' : cs : css
                  , head ts)

    If e1 e2 e3 ->
        do (a1,c1,t1) <- gen e1
           (a2,c2,t2) <- gen e2
           (a3,c3,t3) <- gen e3
           return ( unionsA [a1,a2,a3]
                  , let c4 = Set.fromList [ ctx e1 span (t1 :=: bool)
                                          , ctx' (t2 :=: t3)   ]
                    in  Set.unions [c1,c2,c3,c4]
                  , t2 )

    Data name es ->
        gen $ foldl' (\f x -> epos f x $ App f x) (ctx' $ Var name) es

    Binop op e1 e2 ->
        gen $ ctx' (App (ctx' $ App (ctx' $ Var op) e1) e2)

    Access e label ->
        do (as,cs,rtype) <- gen e
           t <- beta
           rtype' <- beta
           let fs = Map.singleton label [t]
               c = (ctx' (RecordT fs rtype' :=: rtype))
           return (as, Set.insert c cs, t)

    Remove e x -> 
        do (as,cs,rtype) <- gen e
           t <- beta
           rtype' <- beta
           let c = (ctx' (RecordT (Map.singleton x [t]) rtype' :=: rtype))
           return (as, Set.insert c cs, rtype')

    Insert e x v -> 
        do (eas,ecs,etype) <- gen e
           (vas,vcs,vtype) <- gen v
           return ( unionA eas vas
                  , Set.union ecs vcs
                  , RecordT (Map.singleton x [vtype]) etype )

    Modify record fields ->
        do (ras,rcs,rtype) <- gen record
           (ass,css,newTs) <- unzip3 `liftM` mapM gen (map snd fields)
           oldTs <- mapM (\_ -> beta) fields
           rtype' <- beta
           let rT ts = RecordT (recordT (zip (map fst fields) ts)) rtype'
               c = Set.singleton (ctx' (rtype :=: rT oldTs))
           return ( unionsA (ras:ass), Set.unions (c : rcs : css), rT newTs )

    Record fields ->
        let insert label tipe = Map.insertWith (++) label [tipe]
            getScheme (f,args,e) = do
              (as, _, (label, Forall _ cs  tipe)) <- defGenHelp f args e
              return (as, cs, insert label tipe)
        in  do (ass, css, fs) <- unzip3 `liftM` mapM getScheme fields
               return ( unionsA ass
                      , Set.fromList (concat css)
                      , RecordT (foldr ($) Map.empty fs) EmptyRecord )

    Range e1@(C w1 s1 _) e2@(C w2 s2 _) ->
        do (a1,c1,t1) <- gen e1
           (a2,c2,t2) <- gen e2
           return ( unionsA [a1,a2]
                  , Set.unions [ c1, c2, Set.fromList [ C w1 s1 (t1 :=: int)
                                                      , C w1 s2 (t2 :=: int) ] ]
                  , listOf int )

    MultiIf ps -> do (ass,css,t:ts) <- unzip3 `liftM` mapM genPair ps
                     let cs = Set.fromList (map (ctx' . (t :=:)) ts)
                     return (unionsA ass, Set.unions (cs:css), t)
        where genPair (b@(C t s _),e) = do 
                (a1,c1,t1) <- gen b
                (a2,c2,t2) <- gen e
                return ( unionsA [a1,a2]
                       , Set.unions [ c1, c2
                                    , Set.singleton (C t s (t1 :=: bool)) ]
                       , t2 )

    IntNum _ -> do t <- beta
                   return (Map.empty, Set.singleton (ctx' $ t :<: number), t)

    FloatNum _ -> primitive float
    Chr _ -> primitive char
    Str _ -> primitive string
    Boolean _ -> primitive bool
    Markdown _ -> primitive element


primitive :: Type -> GuidCounter (TVarMap, ConstraintSet, Type)
primitive t = return (Map.empty, Set.empty, t)

caseGen :: Type
        -> (Pattern, CExpr)
        -> GuidCounter (TVarMap, ConstraintSet, Type)
caseGen tipe (p, ce@(C _ span e)) = do
  (as ,cs ,t) <- gen ce
  (as',cs',_) <- patternGen (ctx p span) tipe as p
  return ( as', Set.union cs cs', t )

patternGen :: (Constraint -> Context Constraint)
           -> Type     -- Type of e in `case e of ...`
           -> TVarMap
           -> Pattern
           -> GuidCounter (TVarMap, ConstraintSet, Type)
patternGen ctxt tipe as pattern =
  case pattern of
    PAnything -> do b <- beta ; return ( as, Set.empty, b )
    PVar v -> do
      b <- beta
      let cs = map (ctxt . (b :=:) . VarT) (Map.findWithDefault [] v as)
      return ( Map.delete v as, Set.fromList (ctxt (b :=: tipe) : cs), b )
    PData name ps -> do
      constr <- guid
      output <- beta
      let step (as,cs,tipe) p = do b <- beta
                                   (as',cs',t) <- patternGen ctxt b as p
                                   return (as', Set.union cs cs', t ==> tipe)
      (as',cs, t) <- foldM step (as,Set.empty,tipe) (reverse ps)
      return ( Map.insert name [constr] as'
             , Set.insert (ctxt (VarT constr :=: t)) cs
             , output )
    PRecord fs ->
        do pairs <- mapM (\f -> do b <- beta; return (f,b)) fs
           b <- beta
           let t = RecordT (Map.fromList $ map (second (:[])) pairs) b
               mkCs (name,tipe) = map (ctxt . (tipe :=:) . VarT)
                                  (Map.findWithDefault [] name as)
           return ( foldr Map.delete as fs
                  , Set.fromList (ctxt (t :=: tipe) : concatMap mkCs pairs)
                  , t )


defScheme :: Def -> GuidCounter (Map.Map String [X], Scheme)
defScheme def = do (as,cs,hint) <- defGen def
                   return ( as, snd hint )

defGen def = case def of
               FnDef f args e   -> defGenHelp f args e
               OpDef op a1 a2 e -> defGenHelp op [a1,a2] e

defGenHelp name args e = do
  argDict <- mapM (\a -> liftM ((,) a) guid) args 
  (as,cs,t) <- gen e
  let as' = foldr Map.delete as args
      tipe = foldr (==>) t $ map (VarT . snd) argDict
      genCs (arg,x) = do
        v <- guid
        let as' = Map.findWithDefault [v] arg as
        return $ map (\y -> ctx arg NoSpan $ VarT x :=: VarT y) as'
  cs' <- concat `liftM` mapM genCs argDict
  scheme <- generalize (concat $ Map.elems as') $
            Forall (map snd argDict) (cs' ++ Set.toList cs) tipe
  return ( as', Set.empty, (name, scheme) )


stmtGen :: Statement
        -> GuidCounter (TVarMap, ConstraintSet, Map.Map String Scheme)
stmtGen stmt =
  case stmt of
    Definition def -> do (as,cs,hint) <- defGen def
                         return ( as, cs, uncurry Map.singleton hint )

    Datatype name xs tcs ->
        let toScheme ts = Forall xs [] (foldr (==>) (ADT name $ map VarT xs) ts)
        in  return (Map.empty, Set.empty, Map.fromList (map (second toScheme) tcs))

    ExportEvent js elm tipe ->
        do x <- guid
           return ( Map.singleton elm [x]
                  , Set.singleton . ctx elm NoSpan $ VarT x :=: tipe
                  , Map.empty )

    ImportEvent js e@(C txt span base) elm tipe ->
        do (as,cs,t) <- gen e
           return ( as
                  , Set.insert (C txt span (signalOf t :=: tipe)) cs
                  , Map.singleton elm (Forall [] [] tipe) )

    TypeAnnotation name tipe ->
        do schm <- generalize [] (Forall [] [] tipe)
           return (Map.empty, Set.empty, Map.singleton name schm)

    TypeAlias _ _ _ -> return (Map.empty, Set.empty, Map.empty)
