module Type.Solve (solve) where

import Control.Applicative ((<$>), (<*>), (<|>))
import Control.Monad
import Control.Monad.State (StateT, execStateT, liftIO)
import Control.Monad.Except (ExceptT, throwError)
import qualified Data.Foldable as F
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Traversable as T
import qualified Data.UnionFind.IO as UF

import qualified AST.Type as Type
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Type as Error
import qualified Type.State as TS
import Type.Type
import Type.Unify


{-| Every variable has rank less than or equal to the maxRank of the pool.
This sorts variables into the young and old pools accordingly.
-}
generalize :: TS.Pool -> StateT TS.SolverState IO ()
generalize youngPool =
  do  youngMark <- TS.uniqueMark
      let youngRank = TS.maxRank youngPool
      let insert dict var =
            do  descriptor <- liftIO $ UF.descriptor var
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
      let registerIfNotRedundant var =
            do  isRedundant <- liftIO $ UF.redundant var
                if isRedundant then return var else TS.register var

      let rankDict' = Map.delete youngRank rankDict
      T.traverse (mapM registerIfNotRedundant) rankDict'

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
    adjust =
        adjustRank youngMark visitedMark groupRank

    adjustTerm term =
        case term of
          App1 a b ->
              max `liftM` adjust a `ap` adjust b

          Fun1 a b ->
              max `liftM` adjust a `ap` adjust b

          Var1 x ->
              adjust x

          EmptyRecord1 ->
              return outermostRank

          Record1 fields extension ->
              do  ranks <- mapM adjust (concat (Map.elems fields))
                  rnk <- adjust extension
                  return . maximum $ rnk : ranks


-- SOLVER

solve :: TypeConstraint -> ExceptT [A.Located Error.Error] IO TS.SolverState
solve constraint =
  do  state <-
          liftIO (execStateT (actuallySolve constraint) TS.initialState)
      case TS.sError state of
        [] ->
            return state
        errors ->
            throwError errors


actuallySolve :: TypeConstraint -> StateT TS.SolverState IO ()
actuallySolve constraint =
  case constraint of
    CTrue ->
        return ()

    CSaveEnv ->
        TS.saveLocalEnv

    CEqual hint region term1 term2 ->
        do  t1 <- TS.flatten term1
            t2 <- TS.flatten term2
            unify hint region t1 t2

    CAnd cs ->
        mapM_ actuallySolve cs

    CLet [Scheme [] fqs constraint' _] CTrue ->
        do  oldEnv <- TS.getEnv
            mapM TS.introduce fqs
            actuallySolve constraint'
            TS.modifyEnv (\_ -> oldEnv)

    CLet schemes constraint' ->
        do  oldEnv <- TS.getEnv
            headers <- Map.unions `fmap` mapM solveScheme schemes
            TS.modifyEnv $ \env -> Map.union headers env
            actuallySolve constraint'
            mapM occurs $ Map.toList headers
            TS.modifyEnv (\_ -> oldEnv)

    CInstance region name term ->
        do  env <- TS.getEnv
            freshCopy <-
                case Map.lookup name env of
                  Just (A.A _ tipe) ->
                      TS.makeInstance tipe

                  Nothing ->
                      if List.isPrefixOf "Native." name
                        then liftIO (variable Flexible)
                        else error ("Could not find `" ++ name ++ "` when solving type constraints.")

            t <- TS.flatten term
            unify (Error.Instance name) region freshCopy t


solveScheme
    :: TypeScheme
    -> StateT TS.SolverState IO (Map.Map String (A.Located Variable))
solveScheme scheme =
  let
    flatten (A.A region term) =
      A.A region <$> TS.flatten term
  in
  case scheme of
    Scheme [] [] constraint header ->
        do  actuallySolve constraint
            T.traverse flatten header

    Scheme rigidQuantifiers flexibleQuantifiers constraint header ->
        do  let quantifiers = rigidQuantifiers ++ flexibleQuantifiers
            oldPool <- TS.getPool

            -- fill in a new pool when working on this scheme's constraints
            freshPool <- TS.nextRankPool
            TS.switchToPool freshPool
            mapM TS.introduce quantifiers
            header' <- T.traverse flatten header
            actuallySolve constraint

            allDistinct rigidQuantifiers
            youngPool <- TS.getPool
            TS.switchToPool oldPool
            generalize youngPool
            mapM isGeneric rigidQuantifiers
            return header'


-- ADDITIONAL CHECKS

-- Checks that all of the given variables belong to distinct equivalence classes.
-- Also checks that their structure is Nothing, so they represent a variable, not
-- a more complex term.
allDistinct :: [Variable] -> StateT TS.SolverState IO ()
allDistinct vars =
  do  seen <- TS.uniqueMark
      forM_ vars $ \var ->
        do  desc <- liftIO $ UF.descriptor var
            case structure desc of
              Just _ ->
                crash "Cannot generalize something that is not a type variable."

              Nothing ->
                if mark desc == seen
                  then crash "Duplicate variable during generalization."
                  else liftIO (UF.setDescriptor var (desc { mark = seen }))


-- Check that a variable has rank == noRank, meaning that it can be generalized.
isGeneric :: Variable -> StateT TS.SolverState IO ()
isGeneric var =
  do  desc <- liftIO $ UF.descriptor var
      if rank desc == noRank
        then return ()
        else crash "Unable to generalize a type variable. It is not unranked."


crash :: String -> a
crash msg =
  error $
    "It looks like something went wrong with the type inference algorithm.\n"
    ++ msg ++ "\n"
    ++ "Please create a minimal example that triggers this problem and report it to\n"
    ++ "<https://github.com/elm-lang/elm-compiler/issues>"


occurs :: (String, A.Located Variable) -> StateT TS.SolverState IO ()
occurs (name, A.A region variable) =
  do  maybePair <- liftIO $ occursHelp [] variable
      case maybePair of
        Nothing ->
            return ()

        Just (t1, t2) ->
            let
              info = Error.InfiniteTypeInfo name t1 t2
            in
              TS.addError region (Error.InfiniteType info)


occursHelp
    :: [Variable]
    -> Variable
    -> IO (Maybe (Type.Canonical, Type.Canonical))
occursHelp seen var =
  if elem var seen then
      do  desc <- UF.descriptor var
          UF.setDescriptor var (desc { structure = Nothing })
          var' <- UF.fresh desc
          srcVar <- toSrcType var
          srcVar' <- toSrcType var'
          return (Just (srcVar, srcVar'))
  else
      do  desc <- UF.descriptor var
          case structure desc of
            Nothing ->
                return Nothing

            Just struct ->
                let go = occursHelp (var:seen)
                in
                case struct of
                  App1 a b ->
                      (<|>) <$> go a <*> go b

                  Fun1 a b ->
                      (<|>) <$> go a <*> go b

                  Var1 a ->
                      go a

                  EmptyRecord1 ->
                      return Nothing

                  Record1 fields ext ->
                      F.asum <$> mapM go (ext : concat (Map.elems fields))
