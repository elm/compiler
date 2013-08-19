-- This module contains checks to be run *after* type inference has
-- completed successfully. At that point we still need to do occurs
-- checks and ensure that `main` has an acceptable type.
module Type.ExtraChecks where

import qualified Data.Map as Map
import Type.State (Env)

mainCheck :: Env -> IO (Either String Env)
mainCheck env =
    case Map.lookup "main" env of
      Nothing -> return (Right env)
      Just var -> return (Right env)

occursCheck = undefined