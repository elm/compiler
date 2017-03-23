{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Generate.JavaScript (generate) where

import qualified Control.Monad.State as State
import Data.Text (Text)
import qualified Data.ByteString.Builder as BS

import qualified AST.Effects as Effects
import qualified AST.Module as Module
import qualified AST.Module.Name as ModuleName
import qualified Elm.Package as Pkg
import qualified Generate.JavaScript.Builder as JS
import qualified Generate.JavaScript.Expression as JS
import qualified Generate.JavaScript.Helpers as JS
import qualified Generate.JavaScript.Variable as Var



-- GENERATE JAVASCRIPT


generate :: Module.Optimized -> BS.Builder
generate (Module.Module moduleName info) =
  let
    genBody =
      do  defsList <- mapM JS.generateDef (Module.program info)
          let managerStmts = generateEffectManager moduleName (Module.effects info)
          return (concat (defsList ++ [managerStmts]))

    body =
      State.evalState genBody 0
  in
    JS.encodeUtf8 body



-- GENERATE EFFECT MANAGER


generateEffectManager :: ModuleName.Canonical -> Effects.Canonical -> [JS.Stmt]
generateEffectManager moduleName effects =
  case effects of
    Effects.None ->
      []

    Effects.Port _ ->
      []

    Effects.Manager pkgName (Effects.Info _ _ _ _ managerType) ->
      let
        managers =
          Var.coreNative "Platform" "effectManagers"

        managerName =
          JS.String (ModuleName.canonicalToText moduleName)

        entry name =
          ( JS.IdProp (JS.Id name)
          , JS.ref (Var.qualified moduleName name)
          )

        managerEntries =
          [ "pkg" ==> Pkg.toText pkgName
          , entry "init"
          , entry "onEffects"
          , entry "onSelfMsg"
          ]

        otherEntries =
          case managerType of
            Effects.CmdManager _ ->
              [ "tag" ==> "cmd", entry "cmdMap" ]

            Effects.SubManager _ ->
              [ "tag" ==> "sub", entry "subMap" ]

            Effects.FxManager _ _ ->
              [ "tag" ==> "fx", entry "cmdMap", entry "subMap" ]

        addManager =
          JS.Assign
            (JS.LBracket managers managerName)
            (JS.Object (managerEntries ++ otherEntries))
      in
        [ JS.ExprStmt addManager ]


(==>) :: Text -> Text -> ( JS.Prop, JS.Expr )
(==>) key value =
  ( JS.IdProp (JS.Id key)
  , JS.String value
  )
