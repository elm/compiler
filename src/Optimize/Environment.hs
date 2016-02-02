module Optimize.Environment
    ( Optimizer, run
    , Env
    , getVariantDict
    , getHome
    , getTailCall, setTailCall
    , freshName
    ) where

import qualified Control.Monad.State as State

import qualified AST.Module.Name as ModuleName
import qualified Optimize.DecisionTree as DT


type Optimizer a =
    State.State Env a


run :: DT.VariantDict -> ModuleName.Canonical -> Optimizer a -> a
run variantDict home optimizer =
  State.evalState optimizer (Env False 0 variantDict home)


data Env = Env
    { _hasTailCall :: Bool
    , _uid :: Int
    , _variantDict :: DT.VariantDict
    , _home :: ModuleName.Canonical
    }


getVariantDict :: Optimizer DT.VariantDict
getVariantDict =
  State.gets _variantDict


getHome :: Optimizer ModuleName.Canonical
getHome =
  State.gets _home


getTailCall :: Optimizer Bool
getTailCall =
  State.gets _hasTailCall


setTailCall :: Bool -> Optimizer ()
setTailCall bool =
  do  env <- State.get
      State.put (env { _hasTailCall = bool })


freshName :: Optimizer String
freshName =
  do  (Env htc uid vd home) <- State.get
      State.put (Env htc (uid + 1) vd home)
      return ("_p" ++ show uid)

