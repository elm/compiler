module Optimize.Environment
    ( Optimizer, run
    , Env
    , getVariantDict
    , getTailCall, setTailCall
    , freshName
    ) where

import qualified Control.Monad.State as State

import qualified Optimize.Patterns.DecisionTree as DT


type Optimizer a =
    State.State Env a


run :: DT.VariantDict -> Optimizer a -> a
run variantDict optimizer =
  State.evalState optimizer (Env False 0 variantDict)


data Env = Env
    { _hasTailCall :: Bool
    , _uid :: Int
    , _variantDict :: DT.VariantDict
    }


getVariantDict :: Optimizer DT.VariantDict
getVariantDict =
  State.gets _variantDict


getTailCall :: Optimizer Bool
getTailCall =
  State.gets _hasTailCall


setTailCall :: Bool -> Optimizer ()
setTailCall bool =
  do  env <- State.get
      State.put (env { _hasTailCall = bool })


freshName :: Optimizer String
freshName =
  do  (Env htc uid vd) <- State.get
      State.put (Env htc (uid + 1) vd)
      return ("_p" ++ show uid)