module Optimize.Environment where

import qualified Control.Monad.State as State


type Optimizer a =
    State.State Env a


data Env = Env
    { _hasTailCall :: Bool
    , _uid :: Int
    }


setTailCall :: Bool -> Optimizer ()
setTailCall bool =
  do  uid <- State.gets _uid
      State.put (Env bool uid)


freshName :: Optimizer String
freshName =
  do  (Env htc uid) <- State.get
      State.put (Env htc (uid + 1))
      return ("_p" ++ show uid)