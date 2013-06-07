
module Types.Constrain (constrain) where

import Control.Arrow (second)
import Control.Monad (liftM,mapM,zipWithM,foldM)
import Control.Monad.State (evalState)
import Data.Char (isDigit)
import Data.List (foldl',sort,group,isPrefixOf,intercalate,isSuffixOf)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Ast
import Located
import Guid

import Types.Types
import qualified Types.Substitutions as Subs

import System.IO.Unsafe

beta = VarT `liftM` guid
unionA = Map.unionWith (++)
unionsA = Map.unionsWith (++)

getAliases :: [(String,ImportMethod)] -> [(String,Scheme)] -> [(String,Scheme)]
getAliases imports hints = concatMap aliasesFrom imports
    where
      -- Get the names of values that are now in scope after a particular
      -- import statement.
      aliasesFrom :: (String,ImportMethod) -> [(String,Scheme)]
      aliasesFrom (name,method) =
          let values = concatMap (getValue name) hints
              prefixed name = map (\(n,t) -> (name ++ "." ++ n, t)) values
          in  case method of
                As alias -> prefixed alias
                Hiding vs -> prefixed name ++ filter (\(n,t) -> n `notElem` vs) values
                Importing vs -> prefixed name ++ filter (\(n,t) -> n `elem` vs) values

      -- Take a module name and a type annotation. Return the unprefxed
      -- type annotation: getValue "List" ("List.map",t) == [("map",t)]
      getValue :: String -> (String,Scheme) -> [(String,Scheme)]
      getValue inModule (name,tipe) =
          case inModule `isPrefixOf` name of
            True  -> [ (drop (length inModule + 1) name, tipe) ]
            False -> []

findAmbiguous hints assumptions continue =
 let potentialDups = map head . filter (\g -> length g > 1) . group . sort $
                     filter (elem '.') hints
     dups = filter (\k -> Map.member k assumptions) potentialDups
 in case dups of
      n:_ -> return . Left $ "Error: Ambiguous occurrence of '" ++ n ++
             "' could refer to " ++
             intercalate ", " (filter (isSuffixOf n) hints)
      _ -> continue

mergeSchemes :: [Map.Map String Scheme]
             -> GuidCounter (TVarMap, Constraints, Map.Map String Scheme)
mergeSchemes schmss = do (ass,css,sss) <- unzip3 `liftM` mapM split kvs
                         return (Map.unions ass, concat css, Map.unions sss)
  where
    kvs = Map.toList $ Map.unionsWith (++) (map (Map.map (:[])) schmss)
    split (k,vs) =
      let ps = zipWith (\s v -> (s++k,v)) (map (flip replicate '_') [0..]) vs
          eq t u = L (Just $ msg ++ k) NoSpan (VarT t :=: VarT u)
          msg = "the definition of "
      in  do xs <- mapM (\_ -> guid) vs
             return ( Map.fromList $ zip (map fst ps) (map (:[]) xs)
                    , case xs of
                        t:ts -> zipWith eq xs ts
                        []   -> []
                    , Map.fromList ps )

constrain typeHints (Module _ _ imports stmts) = do
  (ass,css,schemess) <- unzip3 `liftM` mapM stmtGen stmts
  aliasHints <- getAliases imports `liftM` typeHints
  (as', cs', schemes) <- mergeSchemes schemess
  let constraints = concat (cs':css)
      as = unionsA (as':ass)
      allHints = Map.union schemes (Map.fromList aliasHints)
      insert as n = do v <- guid; return $ Map.insertWith' (\_ x -> x) n [v] as
  assumptions <- foldM insert as (Map.keys schemes)
  findAmbiguous (map fst aliasHints) assumptions $ do
    let f k s vs = map (\v -> L (Just k) NoSpan $ v :<<: s) vs
        cs = concat . Map.elems $ Map.intersectionWithKey f allHints assumptions
        escapees = Map.keys $ Map.difference assumptions allHints
        msg = "Warning! Type-checker could not find variables:\n" ++ intercalate ", " escapees
    return $ case escapees of
               [] -> Right (constraints ++ cs)
               _  -> unsafePerformIO (putStrLn msg) `seq` Right (constraints ++ cs)
               --_  -> Left ("Undefined variable(s): " ++ intercalate ", " escapees)

type TVarMap = Map.Map String [X]
type Constraints = [Located Constraint]

loc e span = L (Just $ show e) span

gen :: CExpr -> GuidCounter (TVarMap, Constraints, Type)
gen (L _ span expr) =
  let loc' = L (Just $ show expr) span in
  case expr of
    Var x ->
        do b <- guid
           return (Map.singleton x [b], [], VarT b)

    App e1 e2 ->
        do (a1,c1,t1) <- gen e1
           (a2,c2,t2) <- gen e2
           b <- beta
           return ( unionA a1 a2
                  , c1 ++ c2 ++ [loc' $ t1 :=: (LambdaT t2 b)]
                  , b )

    Lambda x e ->
        do (a,c,t) <- gen e
           b <- beta
           v <- guid
           return ( Map.delete x a
                  , (++) c . map (\x -> loc' $ VarT x :=: b) $
                    Map.findWithDefault [v] x a
                  , LambdaT b t )

    Let defs e ->
        do (as,cs,t) <- gen e
           (ass, schemes) <- liftM unzip (mapM defScheme defs)
           let assumptions = unionsA (as:ass)
               getName d = case d of FnDef f _ _    -> f
                                     OpDef op _ _ _ -> op
                                     TypeAnnotation n _ -> n
               names = map getName defs
               genCs name s = do
                 v <- guid
                 let vs = Map.findWithDefault [v] name assumptions
                 return $ map (\x -> loc name span $ x :<<: s) vs
           cs' <- zipWithM genCs names schemes
           return ( foldr Map.delete assumptions names
                  , concat cs' ++ cs
                  , t )

    Case e cases ->
        do (as,cs,t) <- gen e
           (ass,css,ts) <- liftM unzip3 $ mapM (caseGen t) cases
           return ( unionsA $ as:ass
                  , let cases' = map snd cases
                        locs = zipWith epos cases' (tail cases')
                        csts = zipWith (:=:) ts (tail ts)
                        cs' = zipWith ($) locs csts
                    in  concat $ cs' : cs : css
                  , head ts)

    If e1 e2 e3 ->
        do (a1,c1,t1) <- gen e1
           (a2,c2,t2) <- gen e2
           (a3,c3,t3) <- gen e3
           return ( unionsA [a1,a2,a3]
                  , c1 ++ c2 ++ c3 ++ [ loc e1 span (t1 :=: bool), loc' (t2 :=: t3) ]
                  , t2 )

    Data name es ->
        gen $ foldl' (\f x -> epos f x $ App f x) (loc' $ Var name) es

    Binop op e1 e2 ->
        gen $ loc' (App (loc' $ App (loc' $ Var op) e1) e2)

    Access e label ->
        do (as,cs,rtype) <- gen e
           t <- beta
           rtype' <- beta
           let fs = Map.singleton label [t]
               c = (loc' (RecordT fs rtype' :=: rtype))
           return (as, c:cs, t)

    Remove e x -> 
        do (as,cs,rtype) <- gen e
           t <- beta
           rtype' <- beta
           let c = (loc' (RecordT (Map.singleton x [t]) rtype' :=: rtype))
           return (as, c:cs, rtype')

    Insert e x v -> 
        do (eas,ecs,etype) <- gen e
           (vas,vcs,vtype) <- gen v
           return ( unionA eas vas
                  , ecs ++ vcs
                  , RecordT (Map.singleton x [vtype]) etype )

    Modify record fields ->
        do (ras,rcs,rtype) <- gen record
           (ass,css,newTs) <- unzip3 `liftM` mapM gen (map snd fields)
           oldTs <- mapM (\_ -> beta) fields
           rtype' <- beta
           let rT ts = RecordT (recordT (zip (map fst fields) ts)) rtype'
               c = [ loc' (rtype :=: rT oldTs) ]
           return ( unionsA (ras:ass), concat (c : rcs : css), rT newTs )

    Record fields ->
        let insert label tipe = Map.insertWith (++) label [tipe]
            getScheme (f,args,e) = do
              (as, _, (label, Forall _ cs  tipe)) <- defGenHelp f args e
              return (as, cs, insert label tipe)
        in  do (ass, css, fs) <- unzip3 `liftM` mapM getScheme fields
               return ( unionsA ass
                      , concat css
                      , RecordT (foldr ($) Map.empty fs) EmptyRecord )

    Range e1@(L w1 s1 _) e2@(L w2 s2 _) ->
        do (a1,c1,t1) <- gen e1
           (a2,c2,t2) <- gen e2
           return ( unionsA [a1,a2]
                  , c1 ++ c2 ++ [ L w1 s1 (t1 :=: int), L w1 s2 (t2 :=: int) ]
                  , listOf int )

    MultiIf ps -> do (ass,css,t:ts) <- unzip3 `liftM` mapM genPair ps
                     let cs = map (loc' . (t :=:)) ts
                     return (unionsA ass, concat (cs:css), t)
        where genPair (b@(L t s _),e) = do 
                (a1,c1,t1) <- gen b
                (a2,c2,t2) <- gen e
                return ( unionsA [a1,a2]
                       , c1 ++ c2 ++ [ L t s (t1 :=: bool) ]
                       , t2 )

    IntNum _ -> do t <- beta
                   return (Map.empty, [loc' $ t :<: number], t)

    FloatNum _ -> primitive float
    Chr _ -> primitive char
    Str _ -> primitive string
    Boolean _ -> primitive bool
    Markdown _ -> primitive element


primitive :: Type -> GuidCounter (TVarMap, Constraints, Type)
primitive t = return (Map.empty, [], t)

caseGen :: Type
        -> (Pattern, CExpr)
        -> GuidCounter (TVarMap, Constraints, Type)
caseGen tipe (p, ce@(L _ span e)) = do
  (as ,cs ,t) <- gen ce
  (as',cs',_) <- patternGen (loc p span) tipe as p
  return ( as', cs ++ cs', t )

patternGen :: (Constraint -> Located Constraint)
           -> Type     -- Type of e in `case e of ...`
           -> TVarMap
           -> Pattern
           -> GuidCounter (TVarMap, Constraints, Type)
patternGen loc tipe as pattern =
  case pattern of
    PAnything -> do b <- beta ; return ( as, [], b )
    PVar v -> do
      b <- beta
      let cs = map (loc . (b :=:) . VarT) (Map.findWithDefault [] v as)
      return ( Map.delete v as, loc (b :=: tipe) : cs, b )
    PData name ps -> do
      constr <- guid
      output <- beta
      let step (as,cs,tipe) p = do b <- beta
                                   (as',cs',t) <- patternGen loc b as p
                                   return (as', cs ++ cs', t ==> tipe)
      (as',cs, t) <- foldM step (as,[],tipe) (reverse ps)
      return ( Map.insert name [constr] as'
             , loc (VarT constr :=: t) : cs
             , output )
    PRecord fs ->
        do pairs <- mapM (\f -> do b <- beta; return (f,b)) fs
           b <- beta
           let t = RecordT (Map.fromList $ map (second (:[])) pairs) b
               mkCs (name,tipe) = map (loc . (tipe :=:) . VarT)
                                  (Map.findWithDefault [] name as)
           return ( foldr Map.delete as fs
                  , loc (t :=: tipe) : concatMap mkCs pairs
                  , t )


defScheme :: Def -> GuidCounter (Map.Map String [X], Scheme)
defScheme def = do (as,cs,hint) <- defGen def
                   return ( as, snd hint )

defGen def = case def of
               FnDef f args e   -> defGenHelp f args e
               OpDef op a1 a2 e -> defGenHelp op [a1,a2] e
               TypeAnnotation name tipe -> do
                 schm <- Subs.generalize [] =<< Subs.superize name tipe
                 return (Map.empty, [], (name, schm))

defGenHelp name args e = do
  argDict <- mapM (\a -> liftM ((,) a) guid) args 
  (as,cs,t) <- gen e
  let as' = foldr Map.delete as args
      tipe = foldr (==>) t $ map (VarT . snd) argDict
      genCs (arg,x) = do
        v <- guid
        let as' = Map.findWithDefault [v] arg as
        return $ map (\y -> loc arg NoSpan $ VarT x :=: VarT y) as'
  cs' <- concat `liftM` mapM genCs argDict
  scheme <- Subs.generalize (concat $ Map.elems as') $
            Forall (map snd argDict) (cs' ++ cs) tipe
  return ( as', [], (name, scheme) )


stmtGen :: Statement
        -> GuidCounter (TVarMap, Constraints, Map.Map String Scheme)
stmtGen stmt =
  case stmt of
    Definition def -> do (as,cs,hint) <- defGen def
                         return ( as, cs, uncurry Map.singleton hint )

    Datatype name xs tcs ->
        let toScheme ts = Forall xs [] (foldr (==>) (ADT name $ map VarT xs) ts)
        in  return (Map.empty, [], Map.fromList (map (second toScheme) tcs))

    ExportEvent js elm tipe ->
        do x <- guid
           return ( Map.singleton elm [x]
                  , [ loc elm NoSpan $ VarT x :=: tipe ]
                  , Map.empty )

    ImportEvent js e@(L txt span base) elm tipe ->
        do (as,cs,t) <- gen e
           return ( as
                  , L txt span (signalOf t :=: tipe) : cs
                  , Map.singleton elm (Forall [] [] tipe) )

    TypeAlias _ _ _ -> return (Map.empty, [], Map.empty)
