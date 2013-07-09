
module Type.Solve where

import Control.Monad
import Control.Monad.State
import qualified Data.UnionFind.IO as UF
import qualified Data.Array.IO as Array
import qualified Data.Map as Map
import qualified Data.Traversable as Traversable
import qualified Data.Maybe as Maybe
import Type.Type
import Type.Unify
import qualified Type.Environment as Env
import qualified Type.State as TS

register = undefined

generalize :: TS.Pool -> TS.Pool -> IO [Variable]
generalize oldPool youngPool = do
  let young = 0
      visited = 1
      youngRank = TS.maxRank youngPool

  array' <- Array.newArray (0, youngRank) []
  let array = array' :: Array.IOArray Int [Variable]

  -- Insert all of the youngPool variables into the array.
  -- They are placed into a list at the index corresponding
  -- to their rank.
  forM (TS.inhabitants youngPool) $ \var -> do
      desc <- UF.descriptor var
      vars <- Array.readArray array (rank desc)
      Array.writeArray array (rank desc) (var : vars)
  
  -- get the ranks right for each entry
  forM [0 .. youngRank] $ \i -> do
      vars <- Array.readArray array i
      mapM (traverse young visited i) vars

  -- do not need to work with variables that have become redundant
  vars <- Array.readArray array youngRank
  forM vars $ \var -> do
      isRedundant <- UF.redundant var
      if isRedundant then do
          desc <- UF.descriptor var
          if rank desc < youngRank
          then register oldPool var
          else let flex' = if flex desc == Flexible then Rigid else flex desc
               in  UF.setDescriptor var (desc { rank = noRank, flex = flex' })
      else return ()

  return vars

traverse :: Int -> Int -> Int -> Variable -> IO Int
traverse young visited k variable =
    let f = traverse young visited k in
    do desc <- UF.descriptor variable
       case mark desc == young of
         True -> do
           rank' <- case structure desc of
                         Nothing -> return k
                         Just term -> case term of
                                        App1 a b -> max `liftM` f a `ap` f b
                                        Fun1 a b -> max `liftM` f a `ap` f b
                                        Var1 x -> f x
                                        EmptyRecord1 -> return outermostRank
                                        Record1 fields extension -> do
                                            ranks <- mapM f (concat (Map.elems fields))
                                            max (maximum ranks) `liftM` f extension
           UF.setDescriptor variable (desc { mark = visited, rank = rank' })
           return rank'

         False -> do
           if mark desc /= visited then do
               let rank' = min k (rank desc)
               UF.setDescriptor variable (desc { mark = visited, rank = rank' })
               return rank'
           else return (rank desc)

addTo = undefined
newPool = undefined
introduce = undefined

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
        mapM_ introduce fqs
        solve constraint'

    CLet schemes constraint' -> do
        mapM solveScheme schemes
        solve constraint'

    CInstance name term -> do
        let instance' = undefined
            inst = undefined --instance' pool (Env.get env value name)
        t <- TS.flatten term
        unify inst t

solveScheme :: TypeScheme -> StateT TS.SolverState IO ()
solveScheme scheme =
    case scheme of
      Scheme [] [] constraint header -> do
          solve constraint
          Traversable.traverse TS.flatten header
          return ()

      Scheme rigidQuantifiers flexibleQuantifiers constraint header -> do
          let quantifiers = rigidQuantifiers ++ flexibleQuantifiers
          globalPool <- TS.getPool
          localPool <- TS.newPool
          TS.modifyPool (\_ -> localPool)
          mapM TS.introduce quantifiers
          header' <- Traversable.traverse TS.flatten header
          solve constraint
          -- distinct variables
          -- generalize
          -- generic variables
          TS.modifyPool (\_ -> globalPool)

isGeneric var =
    do desc <- UF.descriptor var
       undefined

