module Type.State where

import Type.Type
import qualified Type.Environment as Env
import qualified Data.UnionFind.IO as UF
import Control.Monad.State

-- Pool
-- Holds a bunch of variables
-- The rank of each variable is less than or equal to the pool's "maxRank"
-- The young pool exists to make it possible to identify these vars in constant time.

data Pool = Pool {
  maxRank :: Int,
  inhabitants :: [Variable]
}

-- Keeps track of the environment, type variable pool, and a list of errors
type SolverState = (Env.Environment, Pool, [String])

modifyEnv  f = modify $ \(env, pool, errors) -> (f env, pool, errors)
modifyPool f = modify $ \(env, pool, errors) -> (env, f pool, errors)
addError err = modify $ \(env, pool, errors) -> (env, pool, err:errors)

getPool :: StateT SolverState IO Pool
getPool = do
  (_, pool, _) <- get
  return pool

newPool :: StateT SolverState IO Pool
newPool = do
  (_, pool, _) <- get
  return $ Pool { maxRank = maxRank pool + 1, inhabitants = [] }

register :: Variable -> StateT SolverState IO Variable
register variable = do
    modifyPool $ \pool -> pool { inhabitants = variable : inhabitants pool }
    return variable

introduce :: Variable -> StateT SolverState IO Variable
introduce variable = do
  (_, pool, _) <- get
  liftIO $ UF.modifyDescriptor variable (\desc -> desc { rank = maxRank pool })
  register variable

flatten :: Type -> StateT SolverState IO Variable
flatten term =
  case term of
    VarN v -> return v
    TermN t -> do
      flatStructure <- undefined -- chop t
      (_, pool, _) <- get
      var <- liftIO . UF.fresh $ Descriptor {
               structure = Just flatStructure,
               rank = maxRank pool,
               flex = Flexible,
               name = Nothing,
               mark = 0
             }
      register var