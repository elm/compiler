module Type.Solve (solve) where

import Control.Monad
import Control.Monad.State
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Traversable as Traversable
import qualified Data.UnionFind.IO as UF
import Type.Type
import Type.Unify
import qualified Type.ExtraChecks as Check
import qualified Type.State as TS
import qualified AST.Annotation as A


-- | Every variable has rank less than or equal to the maxRank of the pool.
--   This sorts variables into the young and old pools accordingly.
generalize :: TS.Pool -> StateT TS.SolverState IO ()
generalize youngPool =
  do  youngMark <- TS.uniqueMark 
      let youngRank = TS.maxRank youngPool
          insert dict var = do
            descriptor <- liftIO $ UF.descriptor var
            liftIO $ UF.modifyDescriptor var (\desc -> desc { mark = youngMark })
            return $ Map.insertWith (++) (rank descriptor) [var] dict

      -- Sort the youngPool variables by rank.
      rankDict <- foldM insert Map.empty (TS.inhabitants youngPool)

      -- get the ranks right for each entry.
      -- start at low ranks so that we only have to pass
      -- over the information once.
      visitedMark <- TS.uniqueMark
      forM (Map.toList rankDict) $ \(poolRank, vars) ->
          mapM (adjustRank youngMark visitedMark poolRank) vars

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
            case isRedundant of
              True -> return ()
              False -> do
                desc <- liftIO $ UF.descriptor var
                case rank desc < youngRank of
                  True -> TS.register var >> return ()
                  False -> do
                    let flex' = case flex desc of { Flexible -> Rigid ; other -> other }
                    liftIO $ UF.setDescriptor var (desc { rank = noRank, flex = flex' })
                                     
      mapM_ registerIfLowerRank (Map.findWithDefault [] youngRank rankDict)


-- adjust the ranks of variables such that ranks never increase as you
-- move deeper into a variable.
adjustRank :: Int -> Int -> Int -> Variable -> StateT TS.SolverState IO Int
adjustRank youngMark visitedMark groupRank var =
  do  descriptor <- liftIO $ UF.descriptor var
      case () of
        ()  | mark descriptor == youngMark ->
                do  -- Set the variable as marked first because it may be cyclic.
                    liftIO $ UF.modifyDescriptor var $ \desc ->
                        desc { mark = visitedMark }
                    rank' <- maybe (return groupRank) adjustTerm (structure descriptor)
                    liftIO $ UF.modifyDescriptor var $ \desc ->
                        desc { rank = rank' }
                    return rank'

            | mark descriptor /= visitedMark ->
                do  let rank' = min groupRank (rank descriptor)
                    liftIO $ UF.setDescriptor var (descriptor { mark = visitedMark, rank = rank' })
                    return rank'

            | otherwise ->
                return (rank descriptor)
  where
    adjust = adjustRank youngMark visitedMark groupRank

    adjustTerm term =
        case term of
          App1 a b -> max `liftM` adjust a `ap` adjust b
          Fun1 a b -> max `liftM` adjust a `ap` adjust b
          Var1 x -> adjust x
          EmptyRecord1 -> return outermostRank
          Record1 fields extension ->
              do ranks <- mapM adjust (concat (Map.elems fields))
                 rnk <- adjust extension
                 return . maximum $ rnk : ranks


solve :: TypeConstraint -> StateT TS.SolverState IO ()
solve (A.A region constraint) =
  case constraint of
    CTrue ->
      return ()

    CSaveEnv ->
      TS.saveLocalEnv

    CEqual term1 term2 ->
      do  t1 <- TS.flatten term1
          t2 <- TS.flatten term2
          unify region t1 t2

    CAnd cs ->
      mapM_ solve cs

    CLet [Scheme [] fqs constraint' _] (A.A _ CTrue) ->
      do  oldEnv <- TS.getEnv
          mapM TS.introduce fqs
          solve constraint'
          TS.modifyEnv (\_ -> oldEnv)

    CLet schemes constraint' ->
      do  oldEnv <- TS.getEnv
          headers <- Map.unions `fmap` mapM (solveScheme region) schemes
          TS.modifyEnv $ \env -> Map.union headers env
          solve constraint'
          mapM Check.occurs $ Map.toList headers
          TS.modifyEnv (\_ -> oldEnv)

    CInstance name term ->
      do  env <- TS.getEnv
          freshCopy <-
              case Map.lookup name env of
                Just tipe -> TS.makeInstance tipe
                Nothing
                  | List.isPrefixOf "Native." name -> liftIO (variable Flexible)
                  | otherwise ->
                      error ("Could not find '" ++ name ++ "' when solving type constraints.")

          t <- TS.flatten term
          unify region freshCopy t


solveScheme :: A.Region -> TypeScheme -> StateT TS.SolverState IO (Map.Map String Variable)
solveScheme region scheme =
    case scheme of
      Scheme [] [] constraint header ->
        do  solve constraint
            Traversable.traverse TS.flatten header

      Scheme rigidQuantifiers flexibleQuantifiers constraint header ->
        do  let quantifiers = rigidQuantifiers ++ flexibleQuantifiers
            oldPool <- TS.getPool

            -- fill in a new pool when working on this scheme's constraints
            freshPool <- TS.nextRankPool
            TS.switchToPool freshPool
            mapM TS.introduce quantifiers
            header' <- Traversable.traverse TS.flatten header
            solve constraint

            allDistinct region rigidQuantifiers
            youngPool <- TS.getPool
            TS.switchToPool oldPool
            generalize youngPool
            mapM (isGeneric region) rigidQuantifiers
            return header'


-- Checks that all of the given variables belong to distinct equivalence classes.
-- Also checks that their structure is Nothing, so they represent a variable, not
-- a more complex term.
allDistinct :: A.Region -> [Variable] -> StateT TS.SolverState IO ()
allDistinct region vars =
  do  seen <- TS.uniqueMark
      let check var = do
            desc <- liftIO $ UF.descriptor var
            case structure desc of
              Just _ -> TS.addError region (Just msg) var var
                  where msg = "Cannot generalize something that is not a type variable."

              Nothing -> do
                if mark desc == seen
                  then let msg = "Duplicate variable during generalization."
                       in  TS.addError region (Just msg) var var
                  else return ()
                liftIO $ UF.setDescriptor var (desc { mark = seen })
      mapM_ check vars


-- Check that a variable has rank == noRank, meaning that it can be generalized.
isGeneric :: A.Region -> Variable -> StateT TS.SolverState IO ()
isGeneric region var =
  do  desc <- liftIO $ UF.descriptor var
      if rank desc == noRank
        then return ()
        else
          let msg = "Unable to generalize a type variable. It is not unranked."
          in  TS.addError region (Just msg) var var
