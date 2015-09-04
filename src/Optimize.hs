module Optimize where

import qualified AST.Module as M
import qualified Optimize.TailCalls as TC


optimize :: M.CanonicalModule -> M.Optimized
optimize =
    TC.optimize