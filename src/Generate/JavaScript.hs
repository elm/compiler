{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Generate.JavaScript (generate) where

import Prelude hiding (init)
import Data.Monoid ((<>))
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.ByteString.Builder as BS

import qualified AST.Effects as Effects
import qualified AST.Expression.Optimized as Opt
import qualified AST.Module.Name as ModuleName
import qualified AST.Variable as Var
import qualified Elm.Package as Pkg
import qualified Elm.Compiler.Objects.Internal as Obj
import qualified Generate.JavaScript.Builder as JS
import qualified Generate.JavaScript.Effects as Effects
import qualified Generate.JavaScript.Expression as JS
import qualified Generate.JavaScript.Variable as JS



-- GENERATE JAVASCRIPT


generate :: Obj.SymbolTable -> Obj.Graph -> Obj.Roots -> (Set.Set ModuleName.Canonical, BS.Builder)
generate symbols (Obj.Graph graph) roots =
  let
    (State builders _ natives effects table) =
      List.foldl' (crawl graph) (init symbols) (Obj.toGlobals roots)

    managers =
      fst $ JS.run table (Effects.generate effects)

    javascript =
      List.foldl' (\rest stmt -> JS.encodeUtf8 stmt <> rest) managers builders
  in
    (natives, javascript)



-- DCE STATE


data State =
  State
    { _stmts :: [JS.Stmt]
    , _seen :: Set.Set Var.Global
    , _natives :: Set.Set ModuleName.Canonical
    , _effects :: Map.Map ModuleName.Canonical Effects.ManagerType
    , _table :: JS.Table
    }


init :: Obj.SymbolTable -> State
init symbols =
  State [] Set.empty Set.empty Map.empty (JS.init symbols)



-- DEAD CODE ELIMINATION


type Graph = Map.Map Var.Global Opt.Decl


crawl :: Graph -> State -> Var.Global -> State
crawl graph state@(State js seen natives effects table) name@(Var.Global home _) =
  if Set.member name seen then
    state

  else if home == virtualDomDebug then
    state

  else
    case Map.lookup name graph of
      Just decl ->
        crawlDecl graph name decl state

      Nothing ->
        if ModuleName.canonicalIsNative home then
          State js seen (Set.insert home natives) effects table
        else
          error (crawlError name)


crawlDecl :: Graph -> Var.Global -> Opt.Decl -> State -> State
crawlDecl graph var@(Var.Global home name) (Opt.Decl deps fx body) state =
  let
    newState =
      state { _seen = Set.insert var (_seen state) }

    (State stmts seen natives effects table) =
      Set.foldl' (crawl graph) newState deps

    (stmt, newTable) =
      JS.run table (JS.generateDecl home name body)
  in
    State
      { _stmts = stmt : stmts
      , _seen = seen
      , _natives = natives
      , _effects = maybe id (Map.insert home) fx effects
      , _table = newTable
      }


crawlError :: Var.Global -> String
crawlError (Var.Global (ModuleName.Canonical pkg home) name) =
  Text.unpack $
    "compiler bug manifesting in Generate.JavaScript\n"
    <> "could not find " <> Pkg.toText pkg <> " " <> ModuleName.toText home <> "." <> name <> "\n"
    <> "please report at <https://github.com/elm-lang/elm-compiler/issues>\n"
    <> "try to make an <http://sscce.org/> that demonstrates the issue!"


virtualDomDebug :: ModuleName.Canonical
virtualDomDebug =
  ModuleName.inVirtualDom "VirtualDom.Debug"
