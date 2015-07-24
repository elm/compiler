module Optimize where

import qualified AST.Module as M
import qualified Optimize.TailCalls as TC


optimize :: M.CanonicalModule -> M.Optimized
optimize modul =
    let
        canonicalBody =
            M.body modul

        optimizedProgram =
            TC.optimize (M.program canonicalBody)

        optimizedBody =
            canonicalBody { M.program = optimizedProgram }
    in
        modul { M.body = optimizedBody }