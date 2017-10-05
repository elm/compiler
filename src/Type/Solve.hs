{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Type.Solve (solve) where

import Control.Monad
import Control.Monad.Except (ExceptT, liftIO, throwError)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as MVector

import qualified AST.Module.Name as ModuleName
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Type as Error
import qualified Type.Occurs as Occurs
import qualified Type.State as TS
import Type.Constraint (Constraint(..), Scheme(..))
import Type.Type as Type
import Type.Unify
import qualified Type.UnionFind as UF



-- SOLVER


solve :: Constraint -> ExceptT [A.Located Error.Error] IO TS.State
solve constraint =
  {-# SCC elm_compiler_type_solve #-}
  do  state <- liftIO (TS.run (actuallySolve constraint))
      case TS._errors state of
        [] ->
            return state

        errors ->
            throwError errors


actuallySolve :: Constraint -> TS.Solver ()
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
            TS.introduce fqs
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
                      TS.copy tipe

                  Nothing ->
                      if ModuleName.isKernel name then
                          liftIO mkFlexVar

                      else
                          error ("Could not find `" ++ Text.unpack name ++ "` when solving type constraints.")

            t <- TS.flatten term
            unify (Error.Instance name) region freshCopy t



-- SOLVE SCHEMES


solveScheme :: Scheme -> TS.Solver TS.Env
solveScheme scheme =
  let
    flatten (A.A region term) =
      A.A region <$> TS.flatten term
  in
  case scheme of
    Scheme [] [] constraint header ->
        do  actuallySolve constraint
            traverse flatten header

    Scheme rigidQuantifiers flexQuantifiers constraint header ->
        -- work in the next pool for this scheme's constraints
        solveSchemeHelp rigidQuantifiers flexQuantifiers $
          do  flatHeader <- traverse flatten header
              actuallySolve constraint
              return flatHeader



solveSchemeHelp :: [Variable] -> [Variable] -> TS.Solver a -> TS.Solver a
solveSchemeHelp rigidQuantifiers flexQuantifiers (TS.Solver stepState) =
  TS.Solver $ \state@(TS.SS _ _ rank oldPools _ _) ->
    do
        -- push pool
        let nextRank = rank + 1
        let poolsLength = MVector.length oldPools

        pools <-
          if nextRank < poolsLength
            then return oldPools
            else MVector.grow oldPools poolsLength

        -- introduce quantifiers
        let allQuantifiers = rigidQuantifiers ++ flexQuantifiers
        forM_ allQuantifiers $ \var ->
          UF.modifyDescriptor var $ \(Descriptor content _ mark copy) ->
            Descriptor content nextRank mark copy
        MVector.write pools nextRank allQuantifiers

        -- run solver in next pool
        (value, TS.SS env savedEnv _ _ mark errors) <- stepState (state { TS._rank = nextRank })

        -- pop pool
        let youngMark = mark
        let visitMark = nextMark mark
        let finalMark = nextMark visitMark
        generalize youngMark visitMark nextRank pools
        MVector.write pools nextRank []

        -- check that things went well
        mapM_ isGeneric rigidQuantifiers

        -- return result with pool popped
        return ( value, TS.SS env savedEnv rank pools finalMark errors )


-- Check that a variable has rank == noRank, meaning that it can be generalized.
isGeneric :: Variable -> IO ()
isGeneric var =
  do  (Descriptor _ rank _ _) <- UF.descriptor var
      if rank == noRank
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


occurs :: (Text.Text, A.Located Variable) -> TS.Solver ()
occurs (name, A.A region variable) =
  {-# SCC elm_compiler_type_occurs #-}
  do  hasOccurred <- liftIO $ Occurs.occurs variable
      case hasOccurred of
        False ->
          return ()

        True ->
          do  overallType <- liftIO $ Type.toSrcType variable
              (Descriptor _ rank mark copy) <- liftIO $ UF.descriptor variable
              liftIO $ UF.setDescriptor variable (Descriptor (Error "∞") rank mark copy)
              TS.addError region (Error.InfiniteType (Right name) overallType)



-- GENERALIZE


{-| Every variable has rank less than or equal to the maxRank of the pool.
This sorts variables into the young and old pools accordingly.
-}
generalize :: Mark -> Mark -> Int -> TS.Pools -> IO ()
generalize youngMark visitMark youngRank pools =
  {-# SCC elm_compiler_type_generalize #-}
  do
      youngVars <- MVector.read pools youngRank
      rankTable <- poolToRankTable youngMark youngRank youngVars

      -- get the ranks right for each entry.
      -- start at low ranks so that we only have to pass
      -- over the information once.
      Vector.imapM_ (mapM_ . adjustRank youngMark visitMark) rankTable

      -- For variables that have rank lowerer than youngRank, register them in
      -- the appropriate old pool if they are not redundant.
      Vector.forM_ (Vector.unsafeInit rankTable) $ \vars ->
        forM_ vars $ \var ->
          do  isRedundant <- UF.redundant var
              if isRedundant
                then return ()
                else
                  do  (Descriptor _ rank _ _) <- UF.descriptor var
                      MVector.modify pools (var:) rank

      -- For variables with rank youngRank
      --   If rank < youngRank: register in oldPool
      --   otherwise generalize
      forM_ (Vector.unsafeLast rankTable) $ \var ->
        do  isRedundant <- UF.redundant var
            if isRedundant
              then return ()
              else
                do  (Descriptor content rank mark copy) <- liftIO $ UF.descriptor var
                    if rank < youngRank
                      then
                        MVector.modify pools (var:) rank
                      else
                        UF.setDescriptor var $ Descriptor content noRank mark copy


poolToRankTable :: Mark -> Int -> [Variable] -> IO (Vector.Vector [Variable])
poolToRankTable youngMark youngRank youngInhabitants =
  do  mutableTable <- MVector.replicate (youngRank + 1) []

      -- Sort the youngPool variables into buckets by rank.
      forM_ youngInhabitants $ \var ->
        do  (Descriptor content rank _ copy) <- UF.descriptor var
            UF.setDescriptor var (Descriptor content rank youngMark copy)
            MVector.modify mutableTable (var:) rank

      Vector.unsafeFreeze mutableTable



-- ADJUST RANK

--
-- Adjust variable ranks such that ranks never increase as you move deeper.
-- This way the outermost rank is representative of the entire structure.
--
adjustRank :: Mark -> Mark -> Int -> Variable -> IO Int
adjustRank youngMark visitMark groupRank var =
  {-# SCC elm_compiler_type_adjust #-}
  do  (Descriptor content rank mark copy) <- UF.descriptor var
      if mark == youngMark then
          do  -- Set the variable as marked first because it may be cyclic.
              UF.setDescriptor var $ Descriptor content rank visitMark copy
              maxRank <- adjustRankContent youngMark visitMark groupRank content
              UF.setDescriptor var $ Descriptor content maxRank visitMark copy
              return maxRank

        else if mark == visitMark then
          return rank

        else
          do  let minRank = min groupRank rank
              -- TODO how can minRank ever be groupRank?
              UF.setDescriptor var $ Descriptor content minRank visitMark copy
              return minRank


adjustRankContent :: Mark -> Mark -> Int -> Content -> IO Int
adjustRankContent youngMark visitMark groupRank content =
  let
    go = adjustRank youngMark visitMark groupRank
  in
    case content of
      FlexVar _ ->
          return groupRank

      FlexSuper _ _ ->
          return groupRank

      RigidVar _ ->
          return groupRank

      RigidSuper _ _ ->
          return groupRank

      Structure flatType ->
        case flatType of
          App1 _ args ->
            foldM (\rank arg -> max rank <$> go arg) outermostRank args

          Fun1 arg result ->
              max <$> go arg <*> go result

          EmptyRecord1 ->
              -- THEORY: an empty record never needs to get generalized
              return outermostRank

          Record1 fields extension ->
              do  extRank <- go extension
                  foldM (\rank field -> max rank <$> go field) extRank fields

      Alias _ args _ ->
          -- THEORY: anything in the realVar would be outermostRank
          foldM (\rank (_, argVar) -> max rank <$> go argVar) outermostRank args

      Error _ ->
          return groupRank

