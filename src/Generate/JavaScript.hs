module Generate.JavaScript (generate) where

import qualified Control.Monad.State as State
import qualified Data.Text.Lazy as LazyText
import qualified Language.ECMAScript3.Syntax as JS

import qualified AST.Effects as Effects
import qualified AST.Module as Module
import qualified AST.Module.Name as ModuleName
import qualified Generate.JavaScript.Builder as Builder
import qualified Generate.JavaScript.Expression as JS
import qualified Generate.JavaScript.Helpers as JS
import qualified Generate.JavaScript.Variable as Var



-- GENERATE JAVASCRIPT


generate :: Module.Optimized -> LazyText.Text
generate (Module.Module moduleName _ info) =
  let
    genBody =
      do  defsList <- mapM JS.generateDef (Module.program info)
          let managerStmts = generateEffectManager moduleName (Module.effects info)
          return (concat (defsList ++ [managerStmts]))

    body =
      State.evalState genBody 0
  in
    Builder.stmtsToText body



-- GENERATE EFFECT MANAGER


generateEffectManager :: ModuleName.Canonical -> Effects.Canonical -> [JS.Statement ()]
generateEffectManager moduleName effects =
  case effects of
    Effects.None ->
      []

    Effects.Foreign _ ->
      []

    Effects.Manager (Effects.Info _ _ _ _ managerType) ->
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
            Effects.CmdManager _ ->
              [ tagEntry "cmd", entry "cmdMap" ]

            Effects.SubManager _ ->
              [ tagEntry "sub", entry "subMap" ]

            Effects.FxManager _ _ ->
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
