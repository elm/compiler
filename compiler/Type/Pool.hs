module Type.Pool where

import Type.Type
import qualified Data.UnionFind.IO as UF
import Control.Arrow (first)
import Control.Monad.State

-- Pool
-- Holds a bunch of variables
-- The rank of each variable is less than or equal to the pool's "maxRank"
-- The young pool exists to make it possible to identify these vars in constant time.

-- inhabitants :: Pool -> [Variable]

data Pool = Pool {
  maxRank :: Int,
  inhabitants :: [Variable]
}

register :: Variable -> StateT (Pool,[String]) IO Variable
register variable = do
    modify . first $ \pool -> pool { inhabitants = variable : inhabitants pool }
    return variable

introduce :: Variable -> StateT (Pool,[String]) IO Variable
introduce variable = do
  (pool, _) <- get
  liftIO $ UF.modifyDescriptor variable (\desc -> desc { rank = maxRank pool })
  register variable

flatten :: Type -> StateT (Pool,[String]) IO Variable
flatten term =
  case term of
    VarN v -> return v
    TermN t -> do
      flatStructure <- undefined -- chop t
      (pool, _) <- get
      var <- liftIO . UF.fresh $ Descriptor {
               structure = Just flatStructure,
               rank = maxRank pool,
               flex = Flexible,
               name = Nothing,
               mark = 0
             }
      register var