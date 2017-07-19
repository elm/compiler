{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Generate.JavaScript.Effects
  ( generate
  )
  where


import qualified Data.ByteString.Builder as BS
import qualified Data.Map as Map
import Data.Monoid ((<>))

import qualified AST.Effects as Effects
import qualified AST.Module.Name as ModuleName
import qualified AST.Variable as Var
import qualified Generate.JavaScript.Builder as JS
import qualified Generate.JavaScript.BuiltIn as BuiltIn
import qualified Generate.JavaScript.Variable as Var



-- GENERATE


generate :: Map.Map ModuleName.Canonical Effects.ManagerType -> Var.Generator BS.Builder
generate managers =
  let
    add home mngr makeBuilder =
      do  builder <- makeBuilder
          stmt <- generateManager home mngr
          return (JS.stmtToBuilder stmt <> builder)
  in
    Map.foldrWithKey' add (return "") managers


generateManager :: ModuleName.Canonical -> Effects.ManagerType -> Var.Generator JS.Stmt
generateManager home manager =
  do  managers <- BuiltIn.effectManagers
      let managerName = JS.String (ModuleName.canonicalToText home)
      let lValue = JS.LBracket managers managerName
      entries <- generateEntries home manager
      return $ JS.ExprStmt (JS.Assign lValue entries)


generateEntries :: ModuleName.Canonical -> Effects.ManagerType -> Var.Generator JS.Expr
generateEntries home manager =
  let
    arg name =
      Var.global (Var.Global home name)

    makeArgs =
      case manager of
        Effects.Cmds ->
          [ arg "init", arg "onEffects", arg "onSelfMsg", arg "cmdMap" ]

        Effects.Subs ->
          [ arg "init", arg "onEffects", arg "onSelfMsg", pure (JS.Int 0), arg "subMap" ]

        Effects.Both ->
          [ arg "init", arg "onEffects", arg "onSelfMsg", arg "cmdMap", arg "subMap" ]
  in
    JS.Call
      <$> Var.global createManager
      <*> sequence makeArgs


createManager :: Var.Global
createManager =
  Var.Global (ModuleName.inCore "Elm.Kernel.Platform") "createManager"
