module Generate.JavaScript (generate) where

import qualified Control.Monad.State as State
import qualified Language.ECMAScript3.PrettyPrint as ES
import qualified Language.ECMAScript3.Syntax as JS
import qualified Text.PrettyPrint.Leijen as PP

import qualified AST.Effects as Fx
import qualified AST.Module as Module
import qualified AST.Module.Name as ModuleName
import qualified Generate.JavaScript.Expression as JS
import qualified Generate.JavaScript.Helpers as JS
import qualified Generate.JavaScript.Variable as Var



-- GENERATE JAVASCRIPT


generate :: Module.Optimized -> String
generate (Module.Module moduleName _ info) =
  let
    genBody =
      do  defsList <- mapM JS.generateDef (Module.program info)
          let managerStmts = generateEffectManager moduleName (Module.effects info)
          return (concat (defsList ++ [managerStmts]))

    body =
      State.evalState genBody 0
  in
    PP.displayS (PP.renderPretty 1 160 (ES.prettyPrint body)) ""



-- GENERATE EFFECT MANAGER


generateEffectManager :: ModuleName.Canonical -> Fx.Effects -> [JS.Statement ()]
generateEffectManager moduleName effects =
  case effects of
    Fx.None ->
      []

    Fx.Foreign ->
      []

    Fx.Effect (Fx.Info _ _ _ _ managerType) ->
      let
        managers =
          Var.native (ModuleName.inCore ["Native","Platform"]) "globalManagerInfo"

        managerName =
          JS.StringLit () (ModuleName.canonicalToString moduleName)

        entry name =
          ( JS.PropId () (JS.Id () name)
          , JS.ref (Var.qualified moduleName name)
          )

        managerEntries =
          [ entry "init"
          , entry "onEffects"
          , entry "onSelfMsg"
          ]

        otherEntries =
          case managerType of
            Fx.CmdManager _ ->
              [ tagEntry "cmd", entry "cmdMap" ]

            Fx.SubManager _ ->
              [ tagEntry "sub", entry "subMap" ]

            Fx.FxManager _ _ ->
              [ tagEntry "fx", entry "cmdMap", entry "subMap" ]

        addManager =
          JS.AssignExpr () JS.OpAssign
            (JS.LBracket () managers managerName)
            (JS.ObjectLit () (managerEntries ++ otherEntries))
      in
        [ JS.ExprStmt () addManager ]


tagEntry :: String -> ( JS.Prop (), JS.Expression () )
tagEntry tag =
  ( JS.PropId () (JS.Id () "tag")
  , JS.StringLit () tag
  )
