
module Type.Solve where

import Control.Monad
import Control.Monad.State
import qualified Data.UnionFind.IO as UF
import qualified Data.Map as Map
import qualified Data.Traversable as Traversable
import qualified Data.Maybe as Maybe
import Type.Type
import Type.Unify
import qualified Type.Environment as Env
import qualified Type.State as TS


-- | Every variable has rank less than or equal to the maxRank of the pool.
--   This sorts variables into the young and old pools accordingly.
generalize :: TS.Pool -> StateT TS.SolverState IO ()
generalize youngPool = do
  let youngRank = TS.maxRank youngPool
      insert dict var = do
        desc <- liftIO $ UF.descriptor var
        return $ Map.insertWith (++) (rank desc) [var] dict

  -- Sort the youngPool variables by rank.
  rankDict <- foldM insert Map.empty (TS.inhabitants youngPool)

  -- get the ranks right for each entry.
  -- start at low ranks so that we only have to pass
  -- over the information once.
  youngMark <- TS.uniqueMark 
  visitedMark <- TS.uniqueMark
  Traversable.traverse (mapM (adjustRank youngMark visitedMark youngRank)) rankDict

  -- Move variables out of the young pool if they do not have a young rank.
  -- We should not generalize things we cannot use.
  let youngVars = (Map.!) rankDict youngRank

      registerIfNotRedundant var = do
        isRedundant <- liftIO $ UF.redundant var
        if isRedundant then return var else TS.register var

      registerIfHigherRank var = do
        isRedundant <- liftIO $ UF.redundant var
        if isRedundant then return () else do
            desc <- liftIO $ UF.descriptor var
            if rank desc < youngRank
              then TS.register var >> return ()
              else let flex' = if flex desc == Flexible then Rigid else flex desc
                   in  liftIO $ UF.setDescriptor var (desc { rank = noRank, flex = flex' })

  Traversable.traverse (mapM registerIfNotRedundant) rankDict
  Traversable.traverse (mapM registerIfHigherRank) rankDict

  return ()


-- adjust the ranks of variables such that ranks never increase as you
-- move deeper into a variable. This mean the rank actually represents the
-- deepest variable in the whole type, and we can ignore things at a lower
-- rank than the current constraints.
adjustRank :: Int -> Int -> Int -> Variable -> StateT TS.SolverState IO Int
adjustRank youngMark visitedMark groupRank variable =
    let adjust = adjustRank youngMark visitedMark groupRank in
    do desc <- liftIO $ UF.descriptor variable
       case mark desc == youngMark of
         True -> do
           rank' <- case structure desc of
                         Nothing -> return groupRank
                         Just term -> case term of
                                        App1 a b -> max `liftM` adjust a `ap` adjust b
                                        Fun1 a b -> max `liftM` adjust a `ap` adjust b
                                        Var1 x -> adjust x
                                        EmptyRecord1 -> return outermostRank
                                        Record1 fields extension -> do
                                            ranks <- mapM adjust (concat (Map.elems fields))
                                            max (maximum ranks) `liftM` adjust extension
           liftIO $ UF.setDescriptor variable (desc { mark = visitedMark, rank = rank' })
           return rank'

         False -> do
           if mark desc == visitedMark then return (rank desc) else do
               let rank' = min groupRank (rank desc)
               liftIO $ UF.setDescriptor variable (desc { mark = visitedMark, rank = rank' })
               return rank'


solve :: TypeConstraint -> StateT TS.SolverState IO ()
solve constraint =
  case constraint of
    CTrue -> return ()

    CEqual term1 term2 -> do
        t1 <- TS.flatten term1
        t2 <- TS.flatten term2
        unify t1 t2

    CAnd cs -> mapM_ solve cs

    CLet [Scheme [] fqs constraint' _] CTrue -> do
        mapM_ TS.introduce fqs
        solve constraint'

    CLet schemes constraint' -> do
        headers <- mapM solveScheme schemes
        TS.modifyEnv $ \env -> Map.unions (headers ++ [env])
        solve constraint'

    CInstance name term -> do
        env <- TS.getEnv
        freshCopy <- TS.makeInstance ((Map.!) env name)
        t <- TS.flatten term
        unify freshCopy t

solveScheme :: TypeScheme -> StateT TS.SolverState IO (Map.Map String Variable)
solveScheme scheme =
    case scheme of
      Scheme [] [] constraint header -> do
          solve constraint
          Traversable.traverse TS.flatten header

      Scheme rigidQuantifiers flexibleQuantifiers constraint header -> do
          let quantifiers = rigidQuantifiers ++ flexibleQuantifiers
          currentPool <- TS.getPool

          -- fill in a new pool when working on this scheme's constraints
          emptyPool <- TS.nextRankPool
          TS.switchToPool emptyPool
          mapM TS.introduce quantifiers
          header' <- Traversable.traverse TS.flatten header
          solve constraint

          allDistinct rigidQuantifiers
          localPool <- TS.getPool
          TS.switchToPool currentPool
          generalize localPool
          mapM isGeneric rigidQuantifiers
          return header'


-- Checks that all of the given variables belong to distinct equivalence classes.
-- Also checks that their structure is Nothing, so they represent a variable, not
-- a more complex term.
allDistinct :: [Variable] -> StateT TS.SolverState IO ()
allDistinct vars = do
  seen <- TS.uniqueMark
  let check var = do
        desc <- liftIO $ UF.descriptor var
        case structure desc of
          Just _ -> TS.addError "Cannot generalize something that is not a type variable."
          Nothing -> do
            if mark desc == seen
              then TS.addError "Duplicate variable during generalization"
              else return ()
            liftIO $ UF.setDescriptor var (desc { mark = seen })
  mapM_ check vars

-- Check that a variable has rank == noRank, meaning that it can be generalized.
isGeneric :: Variable -> StateT TS.SolverState IO ()
isGeneric var = do
  desc <- liftIO $ UF.descriptor var
  if rank desc == noRank
    then return ()
    else TS.addError "Cannot generalize. Variable must have not have a rank."