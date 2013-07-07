
module Type.Solve where

import Control.Monad
import qualified Data.UnionFind.IO as UF
import qualified Data.Array.IO as Array
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Type.Type
import Type.Unify
import Type.Environment

-- Pool
-- Holds a bunch of variables
-- The rank of each variable is less than or equal to the pool's "number"
-- The young pool exists to make it possible to identify these vars in constant time.

-- inhabitants :: Pool -> [Variable]

data Pool = Pool {
  maxRank :: Int,
  inhabitants :: [Variable]
}

register = undefined

generalize :: Pool -> Pool -> IO [Variable]
generalize oldPool youngPool = do
  let young = 0
      visited = 1
      youngRank = maxRank youngPool

  array' <- Array.newArray (0, youngRank) []
  let array = array' :: Array.IOArray Int [Variable]

  -- Insert all of the youngPool variables into the array.
  -- They are placed into a list at the index corresponding
  -- to their rank.
  forM (inhabitants youngPool) $ \var -> do
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

success = return []

solve env pool constraint =
    case constraint of
      CTrue -> success

      CEqual term1 term2 ->
          unify (register pool) term1 term2

      CAnd cs -> do
          results <- mapM (solve env pool) cs
          return (concat results)

      CLet schemes constraint' -> do
          env' <- foldM (\env' scheme -> (++) env' `liftM` solveScheme env pool scheme) env schemes
          solve env' pool constraint'

      CInstance name term -> do
          let inst = instance' pool (Env.get env value name)
              t = chop pool term
          unify pool inst t

solveScheme = undefined