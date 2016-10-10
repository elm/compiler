{-# OPTIONS_GHC -Wall #-}
module Type.Solve (solve) where

import Control.Monad
import Control.Monad.State (execStateT, liftIO)
import Control.Monad.Except (ExceptT, throwError)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Foldable as F
import qualified Data.UnionFind.IO as UF

import qualified Reporting.Annotation as A
import qualified Reporting.Error.Type as Error
import qualified Type.State as TS
import Type.Type as Type
import Type.Unify



{-| Every variable has rank less than or equal to the maxRank of the pool.
This sorts variables into the young and old pools accordingly.
-}
generalize :: TS.Pool -> TS.Solver ()
generalize youngPool =
  do  youngMark <- TS.uniqueMark
      let youngRank = TS.maxRank youngPool
      let insert dict var =
            do  descriptor <- liftIO $ UF.descriptor var
                liftIO $ UF.modifyDescriptor var (\desc -> desc { _mark = youngMark })
                return $ Map.insertWith (++) (_rank descriptor) [var] dict

      -- Sort the youngPool variables by rank.
      rankDict <- foldM insert Map.empty (TS.inhabitants youngPool)

      -- get the ranks right for each entry.
      -- start at low ranks so that we only have to pass
      -- over the information once.
      visitedMark <- TS.uniqueMark
      forM_ (Map.toList rankDict) $ \(poolRank, vars) ->
          mapM (adjustRank youngMark visitedMark poolRank) vars

      -- For variables that have rank lowerer than youngRank, register them in
      -- the old pool if they are not redundant.
      let registerIfNotRedundant var =
            do  isRedundant <- liftIO $ UF.redundant var
                if isRedundant then return var else TS.register var

      let rankDict' = Map.delete youngRank rankDict
      F.traverse_ (mapM registerIfNotRedundant) rankDict'

      -- For variables with rank youngRank
      --   If rank < youngRank: register in oldPool
      --   otherwise generalize
      let registerIfLowerRank var = do
            isRedundant <- liftIO $ UF.redundant var
            case isRedundant of
              True -> return ()
              False -> do
                desc <- liftIO $ UF.descriptor var
                case _rank desc < youngRank of
                  True ->
                      TS.register var >> return ()
                  False ->
                      liftIO $ UF.setDescriptor var $ desc
                        { _rank = noRank
                        , _content = rigidify (_content desc)
                        }

      mapM_ registerIfLowerRank (Map.findWithDefault [] youngRank rankDict)


rigidify :: Content -> Content
rigidify content =
  case content of
    Var Flex maybeSuper maybeName ->
        Var Rigid maybeSuper maybeName

    _ ->
        content


-- adjust the ranks of variables such that ranks never increase as you
-- move deeper into a variable.
adjustRank :: Int -> Int -> Int -> Variable -> TS.Solver Int
adjustRank youngMark visitedMark groupRank var =
  do  descriptor <- liftIO $ UF.descriptor var
      adjustRankHelp youngMark visitedMark groupRank var descriptor


adjustRankHelp :: Int -> Int -> Int -> Variable -> Descriptor -> TS.Solver Int
adjustRankHelp youngMark visitedMark groupRank var descriptor =
  if _mark descriptor == youngMark then

      do  -- Set the variable as marked first because it may be cyclic.
          liftIO $ UF.modifyDescriptor var $ \desc ->
              desc { _mark = visitedMark }

          maxRank <-
              adjustRankContent youngMark visitedMark groupRank (_content descriptor)

          liftIO $ UF.modifyDescriptor var $ \desc ->
              desc { _rank = maxRank }

          return maxRank

  else if _mark descriptor /= visitedMark then

      do  let minRank = min groupRank (_rank descriptor)
          liftIO $ UF.setDescriptor var (descriptor { _mark = visitedMark, _rank = minRank })
          return minRank

  else

      return (_rank descriptor)


adjustRankContent :: Int -> Int -> Int -> Content -> TS.Solver Int
adjustRankContent youngMark visitedMark groupRank content =
  let
    go = adjustRank youngMark visitedMark groupRank
  in
    case content of
      Error ->
          return groupRank

      Atom _ ->
          return groupRank

      Var _ _ _ ->
          return groupRank

      Alias _ args realVar ->
          maximum <$> mapM go (realVar : map snd args)

      Structure (App1 func arg) ->
          max <$> go func <*> go arg

      Structure (Fun1 arg result) ->
          max <$> go arg <*> go result

      Structure EmptyRecord1 ->
          return outermostRank

      Structure (Record1 fields extension) ->
          maximum <$> mapM go (extension : Map.elems fields)



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


actuallySolve :: TypeConstraint -> TS.Solver ()
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
            mapM_ TS.introduce fqs
            actuallySolve constraint'
            TS.modifyEnv (\_ -> oldEnv)

    CLet schemes constraint' ->
        do  oldEnv <- TS.getEnv
            headers <- Map.unions <$> mapM solveScheme schemes
            TS.modifyEnv $ \env -> Map.union headers env
            actuallySolve constraint'
            mapM_ occurs $ Map.toList headers
            TS.modifyEnv (\_ -> oldEnv)

    CInstance region name term ->
        do  env <- TS.getEnv
            freshCopy <-
                case Map.lookup name env of
                  Just (A.A _ tipe) ->
                      TS.makeInstance tipe

                  Nothing ->
                      if List.isPrefixOf "Native." name then
                          liftIO (mkVar Nothing)

                      else
                          error ("Could not find `" ++ name ++ "` when solving type constraints.")

            t <- TS.flatten term
            unify (Error.Instance name) region freshCopy t


solveScheme :: TypeScheme -> TS.Solver (Map.Map String (A.Located Variable))
solveScheme scheme =
  let
    flatten (A.A region term) =
      A.A region <$> TS.flatten term
  in
  case scheme of
    Scheme [] [] constraint header ->
        do  actuallySolve constraint
            traverse flatten header

    Scheme rigidQuantifiers flexibleQuantifiers constraint header ->
        do  let quantifiers = rigidQuantifiers ++ flexibleQuantifiers
            oldPool <- TS.getPool

            -- fill in a new pool when working on this scheme's constraints
            freshPool <- TS.nextRankPool
            TS.switchToPool freshPool
            mapM_ TS.introduce quantifiers
            header' <- traverse flatten header
            actuallySolve constraint

            youngPool <- TS.getPool
            TS.switchToPool oldPool
            generalize youngPool
            mapM_ isGeneric rigidQuantifiers
            return header'


-- ADDITIONAL CHECKS


-- Check that a variable has rank == noRank, meaning that it can be generalized.
isGeneric :: Variable -> TS.Solver ()
isGeneric var =
  do  desc <- liftIO $ UF.descriptor var
      if _rank desc == noRank
        then return ()
        else crash "Unable to generalize a type variable. It is not unranked."


crash :: String -> a
crash msg =
  error $
    "It looks like something went wrong with the type inference algorithm.\n\n"
    ++ msg ++ "\n\n"
    ++ "Please create a minimal example that triggers this problem and report it to\n"
    ++ "<https://github.com/elm-lang/elm-compiler/issues>"



-- OCCURS CHECK


occurs :: (String, A.Located Variable) -> TS.Solver ()
occurs (name, A.A region variable) =
  do  hasOccurred <- liftIO $ occursHelp [] variable
      case hasOccurred of
        False ->
            return ()

        True ->
            do  overallType <- liftIO (Type.toSrcType variable)
                TS.addError region (Error.InfiniteType name overallType)


occursHelp :: [Variable] -> Variable -> IO Bool
occursHelp seen var =
  if elem var seen then
      do  infiniteDescriptor <- UF.descriptor var
          UF.setDescriptor var (infiniteDescriptor { _content = Error })
          return True

  else
      do  desc <- UF.descriptor var
          case _content desc of
            Atom _ ->
                return False

            Var _ _ _ ->
                return False

            Error ->
                return False

            Alias _ args _ ->
                -- TODO is it okay to only check args?
                or <$> mapM (occursHelp (var:seen) . snd) args

            Structure term ->
                let
                  go = occursHelp (var:seen)
                in
                case term of
                  App1 a b ->
                      (||) <$> go a <*> go b

                  Fun1 a b ->
                      (||) <$> go a <*> go b

                  EmptyRecord1 ->
                      return False

                  Record1 fields ext ->
                      or <$> mapM go (ext : Map.elems fields)
