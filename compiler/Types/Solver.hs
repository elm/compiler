
module Types.Solver (solver) where

import Context
import Control.Arrow (second)
import Control.Monad (liftM)
import Data.Either (lefts,rights)
import Data.List (foldl')
import Data.Maybe (isJust)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Guid
import Types.Types
import Types.Substitutions
import Types.Alias (dealias)

isSolved ss (C _ _ (t1 :=: t2)) = t1 == t2
isSolved ss (C _ _ (x :<<: _)) = isJust (lookup x ss)
isSolved ss c = False

type Aliases = Map.Map String ([X],Type)

crush :: Aliases -> Scheme -> GuidCounter (Either String Scheme)
crush aliases (Forall xs cs t) =
    do subs <- solver aliases Map.empty cs
       return $ do ss' <- subs
                   let ss  = Map.toList ss'
                       cs' = filter (not . isSolved ss) (subst ss cs)
                   return $ Forall xs cs' (subst ss t)

schemeSubHelp txt span x s t1 rltn t2 = do
  (t1',cs1) <- sub t1
  (t2',cs2) <- sub t2
  return (C txt span (rltn t1' t2') : cs1 ++ cs2)
      where sub t | not (occurs x t) = return (t, [])
                  | otherwise = do (st, cs) <- concretize s
                                   return (subst [(x,st)] t, cs)

schemeSub aliases x s c =
    do s' <- crush aliases s
       case s' of
         Right s'' -> Right `liftM` schemeSub' x s'' c
         Left err  -> return $ Left err

schemeSub' x s c@(C txt span constraint) =
  case constraint of
    (t1 :=: t2) -> schemeSubHelp txt span x s t1 (:=:) t2
    (t1 :<: t2) -> schemeSubHelp txt span x s t1 (:<:) t2
    (y :<<: Forall cxs ccs ctipe)
        | not (occurs x c) -> return [c]
        | otherwise ->
            do Forall xs cs tipe <- rescheme s
               let ss = [(x,tipe)]
                   constraints = subst ss (cs ++ ccs)
                   c' = y :<<: Forall (cxs ++ xs) constraints (subst ss ctipe)
               return [ C txt span c' ]

recordConstraints eq fs t fs' t' =
  liftM concat . sequence $
      [ constrain fs fs'
      , liftM concat . mapM (\(k,ts) -> zipper [] k ts []) . Map.toList $
              Map.difference fs fs'
      , liftM concat . mapM (\(k,ts) -> zipper [] k [] ts) . Map.toList $
              Map.difference fs' fs
      ]
    where constrain :: Map.Map String [Type] -> Map.Map String [Type]
                    -> GuidCounter [Context Constraint]
          constrain as bs = liftM concat . sequence . Map.elems $
                            Map.intersectionWithKey (zipper []) as bs
          zipper :: [Context Constraint] -> String -> [Type] -> [Type]
                 -> GuidCounter [Context Constraint]
          zipper cs k xs ys =
            case (xs,ys) of
              (a:as, b:bs) -> zipper (eq a b : cs) k as bs
              ([],[]) -> return cs
              (as,[]) -> do x <- guid
                            let tipe = RecordT (Map.singleton k as) (VarT x)
                            return (cs ++ [eq t' tipe])
              ([],bs) -> do x <- guid
                            let tipe = RecordT (Map.singleton k bs) (VarT x)
                            return (cs ++ [eq t tipe])
                              
solver :: Aliases
       -> Map.Map X Type
       -> [Context Constraint]
       -> GuidCounter (Either String (Map.Map X Type))
solver _ subs [] = return $ Right subs
solver aliases subs (C txt span c : cs) =
  let ctx = C txt span
      eq t1 t2 = ctx (t1 :=: t2)
      solv = solver aliases subs
      uniError' = uniError (\t1 t2 -> solv (eq t1 t2 : cs)) aliases txt span
  in case c of
      -- Destruct Type-constructors
      t1@(ADT n1 ts1) :=: t2@(ADT n2 ts2) ->
          if n1 == n2 then solv (zipWith eq ts1 ts2 ++ cs)
                      else uniError' t1 t2

      LambdaT t1 t2 :=: LambdaT t1' t2' ->
          solv ([ eq t1 t1', eq t2 t2' ] ++ cs)

      RecordT fs t :=: RecordT fs' t' ->
          do cs' <- recordConstraints eq fs t fs' t'
             solv (cs' ++ cs)

      -- Type-equality
      VarT x :=: VarT y
          | x == y    -> solv cs
          | otherwise ->
              case (Map.lookup x subs, Map.lookup y subs) of
                (Just (Super xts), Just (Super yts)) ->
                    let ts = Set.intersection xts yts
                        setXY t = Map.insert x t . Map.insert y t
                    in  case Set.toList ts of
                          []  -> unionError txt span xts yts
                          [t] -> let cs1 = subst [(x,t),(y,t)] cs in
                                 cs1 `seq` solver aliases (setXY t subs) cs1
                          _   -> solver aliases (setXY (Super ts) subs) cs
                (Just (Super xts), _) ->
                    let cs2 = subst [(y,VarT x)] cs in
                    solver aliases (Map.insert y (VarT x) subs) cs2
                (_, _) ->
                    let cs3 = subst [(x,VarT y)] cs in
                    solver aliases (Map.insert x (VarT y) subs) cs3

      VarT x :=: t -> do
          if x `occurs` t then occursError txt span (VarT x) t else
            (case Map.lookup x subs of
               Nothing ->
                   let cs4 = subst [(x,t)] cs
                       subs' = Map.map (subst [(x,t)]) $ Map.insert x t subs
                   in  solver aliases subs' cs4
               Just (Super ts) ->
                   let ts' = Set.intersection ts (Set.singleton t) in
                   case Set.toList ts' of
                     []   -> solv (ctx (t :<: Super ts) : cs)
                     [t'] -> let cs5 = subst [(x,t)] cs in
                             solver aliases (Map.insert x t' subs) cs5
                     _    -> solver aliases (Map.insert x (Super ts') subs) cs
               Just t' -> solv (ctx (t' :=: t) : cs)
            )

      t :=: VarT x -> solv ((ctx (VarT x :=: t)) : cs)

      t1 :=: t2 | t1 == t2  -> solv cs
                | otherwise -> uniError' t1 t2

      -- subtypes
      VarT x :<: Super ts ->
          case Map.lookup x subs of
            Nothing -> solver aliases (Map.insert x (Super ts) subs) cs
            Just (Super ts') ->
                case Set.toList $ Set.intersection ts ts' of
                  []   -> unionError txt span ts ts'
                  [t]  -> solver aliases (Map.insert x t subs) (subst [(x,t)] cs)
                  ts'' -> solver aliases subs' cs
                    where subs' = Map.insert x (Super $ Set.fromList ts'') subs

      ADT "List" [t] :<: Super ts
          | any f (Set.toList ts) -> solv cs
          | otherwise -> subtypeError txt span (ADT "List" [t]) (Super ts)
                 where f (ADT "List" [VarT _]) = True
                       f (ADT "List" [t']) = dealias aliases t == t'
                       f _ = False

      t :<: Super ts
          | Set.member t ts -> solv cs
          | Set.member (dealias aliases t) ts -> solv cs
          | otherwise -> subtypeError txt span t (Super ts)

      x :<<: s
          | any (occurs x) cs ->
              do css <- mapM (schemeSub aliases x s) cs
                 case lefts css of
                   err : _ -> return $ Left err
                   [] -> solv (concat (rights css))
          | otherwise ->
              do (t,cs7) <- concretize s
                 solv (cs ++ ctx (VarT x :=: t) : cs7)

showMsg msg = case msg of
                Just str -> "\nIn context: " ++ str
                Nothing  -> ""

occursError msg span t1 t2 =
    return . Left $ concat
        [ "Type error (" ++ show span ++ "):\n"
        , "Occurs check: cannot construct the infinite type:\n"
        , show t1, " = ", show t2, showMsg msg ]

uniError solveWith aliases msg span t1 t2 =
    let t1' = dealias aliases t1
        t2' = dealias aliases t2
    in  if t1 /= t1' || t2 /= t2'
        then solveWith t1' t2'
        else return . Left $ concat
                 [ "Type error (" ++ show span ++ "):\n"
                 , show t1, " is not equal to ", show t2, showMsg msg ]

unionError msg span ts ts' =
    return . Left $ concat
        [ "Type error (" ++ show span ++ "):\n"
        , "There are no types in both "
        , show (Super ts), " and ", show (Super ts'), showMsg msg ]

subtypeError msg span t s =
    return . Left $ concat
        [ "Type error (" ++ show span ++ "):\n"
        , show t, " is not a ", show s, showMsg msg ]

