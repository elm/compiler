{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Generate.JavaScript (generate) where

import qualified Control.Monad.State as State
import Data.Monoid ((<>))
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.ByteString.Builder as BS

import qualified AST.Effects as Effects
import qualified AST.Expression.Optimized as Opt
import qualified AST.Module.Name as ModuleName
import qualified AST.Variable as Var
import qualified Elm.Package as Pkg
import qualified Generate.JavaScript.Builder as JS
import qualified Generate.JavaScript.Expression as JS
import qualified Generate.JavaScript.Helpers as JS
import qualified Generate.JavaScript.Variable as JS



-- GENERATE JAVASCRIPT


type Graph =
  Map.Map Var.Global Opt.Decl


generate :: Graph -> [Var.Global] -> (Set.Set ModuleName.Canonical, BS.Builder)
generate graph roots =
  let
    (State builders _ natives effects) =
      List.foldl' (crawl graph) empty roots

    managers =
      Map.foldrWithKey addManager "" effects

    javascript =
      List.foldl' (\rest js -> js <> rest) managers builders
  in
    (natives, javascript)



-- DEAD CODE ELIMINATION


data State =
  State
    { _js :: [BS.Builder]
    , _seen :: Set.Set Var.Global
    , _natives :: Set.Set ModuleName.Canonical
    , _effects :: Map.Map ModuleName.Canonical Effects.ManagerType
    }


empty :: State
empty =
  State [] Set.empty Set.empty Map.empty



-- DEAD CODE ELIMINATION


crawl :: Graph -> State -> Var.Global -> State
crawl graph state@(State js seen natives effects) name@(Var.Global home _) =
  if Set.member name seen then
    state

  else
    case Map.lookup name graph of
      Just decl ->
        crawlDecl graph name decl state

      Nothing ->
        if ModuleName.canonicalIsNative home then
          State js seen (Set.insert home natives) effects
        else
          error "TODO something went wrong"


crawlDecl :: Graph -> Var.Global -> Opt.Decl -> State -> State
crawlDecl graph name@(Var.Global home _) (Opt.Decl deps mfx body) state =
  let
    newState =
      state { _seen = Set.insert name (_seen state) }

    (State js seen natives effects) =
      Set.foldl' (crawl graph) newState deps

    newEffects =
      case mfx of
        Nothing -> effects
        Just fx -> Map.insert home fx effects
  in
    State
      { _js = generateDecl name body : js
      , _seen = seen
      , _natives = natives
      , _effects = newEffects
      }


generateDecl :: Var.Global -> Opt.Def -> BS.Builder
generateDecl (Var.Global home name) def =
  let
    genBody =
      JS.generateDecl home name def
  in
    JS.encodeUtf8 (State.evalState genBody 0)



-- ADD EFFECT MANAGER


addManager :: ModuleName.Canonical -> Effects.ManagerType -> BS.Builder -> BS.Builder
addManager name manager builder =
  JS.encodeUtf8 [genManager name manager] <> builder


genManager :: ModuleName.Canonical -> Effects.ManagerType -> JS.Stmt
genManager moduleName@(ModuleName.Canonical pkg _) manager =
  let
    managers =
      JS.coreNative "Platform" "effectManagers"

    managerName =
      JS.String (ModuleName.canonicalToText moduleName)

    entry name =
      ( JS.IdProp (JS.Id name)
      , JS.ref (JS.qualified moduleName name)
      )

    managerEntries =
      [ "pkg" ==> Pkg.toText pkg
      , entry "init"
      , entry "onEffects"
      , entry "onSelfMsg"
      ]

    otherEntries =
      case manager of
        Effects.Cmds -> [ "tag" ==> "cmd", entry "cmdMap" ]
        Effects.Subs -> [ "tag" ==> "sub", entry "subMap" ]
        Effects.Both -> [ "tag" ==> "fx", entry "cmdMap", entry "subMap" ]
  in
    JS.ExprStmt $
      JS.Assign
        (JS.LBracket managers managerName)
        (JS.Object (managerEntries ++ otherEntries))


(==>) :: Text -> Text -> ( JS.Prop, JS.Expr )
(==>) key value =
  ( JS.IdProp (JS.Id key)
  , JS.String value
  )
