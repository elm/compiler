module Generate.JavaScript (generate) where

import Control.Monad.State as State
import Language.ECMAScript3.PrettyPrint as ES
import qualified Text.PrettyPrint.Leijen as PP

import qualified AST.Module as Module
import qualified Generate.JavaScript.Expression as JS


generate :: Module.Optimized -> String
generate (Module.Module _ _ info) =
  let
    genBody =
      concat <$> mapM JS.generateDef (Module.program info)

    body =
      State.evalState genBody 0
  in
    PP.displayS (PP.renderPretty 1 160 (ES.prettyPrint body)) ""

