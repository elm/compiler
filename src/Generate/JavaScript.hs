module Generate.JavaScript (generate) where

import Control.Monad.State as State
import Language.ECMAScript3.PrettyPrint as ES
import Language.ECMAScript3.Syntax
import qualified Text.PrettyPrint.Leijen as PP

import qualified AST.Module as Module
import qualified Generate.JavaScript.Expression as JS
import qualified Generate.JavaScript.Variable as Var


generate :: Module.Optimized -> String
generate module_ =
  let
    definitions =
        Module.program (Module.body module_)

    setup =
        Var.define "_op" (ObjectLit () [])

    stmts =
        State.evalState (mapM JS.generateDef definitions) 0
  in
    PP.displayS (PP.renderPretty 0.4 160 (ES.prettyPrint (setup:stmts))) ""
