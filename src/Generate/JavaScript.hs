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
    (State builders _ kernels effects table) =
      List.foldl' (crawl graph) (init symbols) (Obj.toGlobals roots)

    managers =
      fst $ JS.run table (Effects.generate effects)

    javascript =
      List.foldl' (\rest stmt -> JS.encodeUtf8 stmt <> rest) managers builders
  in
    (kernels, javascript)



-- DCE STATE


data State =
  State
    { _stmts :: [JS.Stmt]
    , _seen :: Set.Set Var.Global
    , _kernels :: Set.Set ModuleName.Canonical
    , _effects :: Map.Map ModuleName.Canonical Effects.ManagerType
    , _table :: JS.Table
    }


init :: Obj.SymbolTable -> State
init symbols =
  State [] Set.empty Set.empty Map.empty (JS.init symbols)



-- DEAD CODE ELIMINATION


type Graph = Map.Map Var.Global Opt.Decl


crawl :: Graph -> State -> Var.Global -> State
crawl graph state@(State js seen kernels effects table) name@(Var.Global home _) =
  if Set.member name seen then
    state

  else if home == virtualDomDebug then
    state

  else
    case Map.lookup name graph of
      Just decl ->
        crawlDecl graph name decl state

      Nothing ->
        if ModuleName.canonicalIsKernel home then
          State js seen (Set.insert home kernels) effects table
        else
          error (crawlError name)


crawlDecl :: Graph -> Var.Global -> Opt.Decl -> State -> State
crawlDecl graph var@(Var.Global home name) (Opt.Decl direct indirect fx body) state1 =
  let
    state2 =
      state1 { _seen = Set.insert var (_seen state1) }

    (State stmts seen kernels effects table) =
      Set.foldl' (crawl graph) state2 direct

    (stmt, newTable) =
      JS.run table (JS.generateDecl home name body)

    state4 =
      State
        { _stmts = stmt : stmts
        , _seen = seen
        , _kernels = kernels
        , _effects = maybe id (Map.insert home) fx effects
        , _table = newTable
        }
  in
    Set.foldl' (crawl graph) state4 indirect


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
