{-# LANGUAGE MultiWayIf #-}
module Type.Solve (solve) where

import Control.Monad
import Control.Monad.State
import qualified Data.UnionFind.IO as UF
import qualified Data.Map as Map
import qualified Data.Traversable as Traversable
import qualified Data.Maybe as Maybe
import qualified Data.List as List
import Type.Type
import Type.Unify
import qualified Type.Environment as Env
import qualified Type.State as TS
import qualified Text.PrettyPrint as P
import SourceSyntax.Location (Located(L))


-- | Every variable has rank less than or equal to the maxRank of the pool.
--   This sorts variables into the young and old pools accordingly.
generalize :: TS.Pool -> StateT TS.SolverState IO ()
generalize youngPool = do
  youngMark <- TS.uniqueMark 
  let youngRank = TS.maxRank youngPool
      insert dict var = do
        desc <- liftIO $ UF.descriptor var
        liftIO $ UF.modifyDescriptor var (\desc -> desc { mark = youngMark })
        return $ Map.insertWith (++) (rank desc) [var] dict

  -- Sort the youngPool variables by rank.
  rankDict <- foldM insert Map.empty (TS.inhabitants youngPool)

  -- get the ranks right for each entry.
  -- start at low ranks so that we only have to pass
  -- over the information once.
  visitedMark <- TS.uniqueMark
  mapM (\(poolRank, vars) -> mapM (adjustRank youngMark visitedMark poolRank) vars) (Map.toList rankDict)

  -- For variables that have rank lowerer than youngRank, register them in
  -- the old pool if they are not redundant.
  let registerIfNotRedundant var = do
        isRedundant <- liftIO $ UF.redundant var
        if isRedundant then return var else TS.register var

  let rankDict' = Map.delete youngRank rankDict
  Traversable.traverse (mapM registerIfNotRedundant) rankDict'

  -- For variables with rank youngRank
  --   If rank < youngRank: register in oldPool
  --   otherwise generalize
  let registerIfLowerRank var = do
        isRedundant <- liftIO $ UF.redundant var
        if isRedundant then return () else do
            desc <- liftIO $ UF.descriptor var
            if rank desc < youngRank
              then TS.register var >> return ()
              else let flex' = if flex desc == Flexible then Rigid else flex desc
                   in  liftIO $ UF.setDescriptor var (desc { rank = noRank, flex = flex' })

  mapM registerIfLowerRank (Map.findWithDefault [] youngRank rankDict)

  return ()


-- adjust the ranks of variables such that ranks never increase as you
-- move deeper into a variable.
adjustRank :: Int -> Int -> Int -> Variable -> StateT TS.SolverState IO Int
adjustRank youngMark visitedMark groupRank variable =
    let adjust = adjustRank youngMark visitedMark groupRank in
    do desc <- liftIO $ UF.descriptor variable
       if | mark desc == youngMark -> do
              rank' <- case structure desc of
                         Nothing -> return groupRank
                         Just term -> case term of
                                        App1 a b -> max `liftM` adjust a `ap` adjust b
                                        Fun1 a b -> max `liftM` adjust a `ap` adjust b
                                        Var1 x -> adjust x
                                        EmptyRecord1 -> return outermostRank
                                        Record1 fields extension -> do
                                            ranks <- mapM adjust (concat (Map.elems fields))
                                            rnk <- adjust extension
                                            return . maximum $ rnk : ranks
              liftIO $ UF.setDescriptor variable (desc { mark = visitedMark, rank = rank' })
              return rank'

          | mark desc /= visitedMark -> do
              let rank' = min groupRank (rank desc)
              liftIO $ UF.setDescriptor variable (desc { mark = visitedMark, rank = rank' })
              return rank'

          | otherwise -> return (rank desc)



solve :: TypeConstraint -> StateT TS.SolverState IO ()
solve (L span constraint) =
  case constraint of
    CTrue -> return ()

    CSaveEnv -> TS.saveLocalEnv

    CEqual term1 term2 -> do
        t1 <- TS.flatten term1
        t2 <- TS.flatten term2
        unify t1 t2

    CAnd cs -> mapM_ solve cs

    CLet [Scheme [] fqs constraint' _] (L _ CTrue) -> do
        oldEnv <- TS.getEnv
        mapM TS.introduce fqs
        solve constraint'
        TS.modifyEnv (\_ -> oldEnv)

    CLet schemes constraint' -> do
        oldEnv <- TS.getEnv
        headers <- mapM solveScheme schemes
        TS.modifyEnv $ \env -> Map.unions (headers ++ [env])
        solve constraint'
        TS.modifyEnv (\_ -> oldEnv)

    CInstance name term -> do
        env <- TS.getEnv
        freshCopy <-
            case Map.lookup name env of
              Just tipe -> TS.makeInstance tipe
              Nothing
                | List.isPrefixOf "Native." name -> liftIO (var Flexible)
                | otherwise ->
                    error ("Could not find '" ++ name ++ "' when solving type constraints.")

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
          oldPool <- TS.getPool

          -- fill in a new pool when working on this scheme's constraints
          freshPool <- TS.nextRankPool
          TS.switchToPool freshPool
          mapM TS.introduce quantifiers
          header' <- Traversable.traverse TS.flatten header
          solve constraint

          allDistinct rigidQuantifiers
          youngPool <- TS.getPool
          TS.switchToPool oldPool
          generalize youngPool
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
          Just _ ->
              TS.addError "Cannot generalize something that is not a type variable" var var
          Nothing -> do
            if mark desc == seen
              then TS.addError "Duplicate variable during generalization" var var
              else return ()
            liftIO $ UF.setDescriptor var (desc { mark = seen })
  mapM_ check vars

-- Check that a variable has rank == noRank, meaning that it can be generalized.
isGeneric :: Variable -> StateT TS.SolverState IO ()
isGeneric var = do
  desc <- liftIO $ UF.descriptor var
  if rank desc == noRank
    then return ()
    else TS.addError "Cannot generalize. Variable must have not have a rank." var var