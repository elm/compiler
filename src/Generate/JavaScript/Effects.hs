{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Generate.JavaScript.Effects
  ( generate
  )
  where


import qualified Data.ByteString.Builder as BS
import qualified Data.Map as Map
import Data.Monoid ((<>))
import Data.Text (Text)

import qualified AST.Effects as Effects
import qualified AST.Module.Name as ModuleName
import qualified AST.Variable as Var
import qualified Elm.Package as Pkg
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
          return (JS.encodeUtf8 stmt <> builder)
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
generateEntries home@(ModuleName.Canonical pkg _) manager =
  let
    entry name =
      do  let key = JS.IdProp (JS.Id name)
          value <- Var.global (Var.Global home name)
          return ( key, value )
  in
    fmap JS.Object $ sequence $
      [ "pkg" ==> Pkg.toText pkg
      , entry "init"
      , entry "onEffects"
      , entry "onSelfMsg"
      ]
      ++
        case manager of
          Effects.Cmds ->
            [ "tag" ==> "cmd"
            , entry "cmdMap"
            ]

          Effects.Subs ->
            [ "tag" ==> "sub"
            , entry "subMap"
            ]

          Effects.Both ->
            [ "tag" ==> "fx"
            , entry "cmdMap"
            , entry "subMap"
            ]


(==>) :: Text -> Text -> Var.Generator ( JS.Prop, JS.Expr )
(==>) key value =
  return ( JS.IdProp (JS.Id key), JS.String value )
