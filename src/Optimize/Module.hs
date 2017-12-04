{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Optimize.Module
  ( Graph(..)
  , optimize
  )
  where


import Prelude hiding (cycle)
import Control.Monad (foldM)
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified AST.Canonical as Can
import qualified AST.Optimized as Opt
import qualified AST.Module.Name as ModuleName
import qualified Elm.Name as N
import qualified Optimize.Expression as Expr
import qualified Optimize.Names as Names
import qualified Optimize.Port as Port
import qualified Reporting.Annotation as A



-- OPTIMIZE


data Graph =
  Graph
    { _fields :: Map.Map N.Name Int
    , _nodes :: Nodes
    }


type Nodes =
  Map.Map Opt.Global Opt.Node


optimize :: Can.Module -> Graph
optimize (Can.Module home _ _ decls unions aliases _ effects) =
  addEffects home effects $
    addUnions home unions $
      addAliases home aliases $
        addDecls home decls $
          Graph Map.empty Map.empty



-- UNION


addUnions :: ModuleName.Canonical -> Map.Map N.Name Can.Union -> Graph -> Graph
addUnions home unions (Graph fields nodes) =
  Graph fields $
    Map.foldr (addUnion home) nodes unions


addUnion :: ModuleName.Canonical -> Can.Union -> Nodes -> Nodes
addUnion home (Can.Union _ ctors) nodes =
  foldr (addCtor home) nodes ctors


addCtor :: ModuleName.Canonical -> (N.Name, [t]) -> Nodes -> Nodes
addCtor home (ctor, args) nodes =
  let
    argNames =
      map N.localFromInt [ 1 .. length args ]

    function =
      Opt.Function argNames $
        Opt.Ctor ctor (map Opt.VarLocal argNames)

    node =
      Opt.Define function Set.empty Set.empty
  in
  Map.insert (Opt.Global home ctor) node nodes



-- ALIAS


addAliases :: ModuleName.Canonical -> Map.Map N.Name Can.Alias -> Graph -> Graph
addAliases home aliases graph =
  Map.foldrWithKey (addAlias home) graph aliases


addAlias :: ModuleName.Canonical -> N.Name -> Can.Alias -> Graph -> Graph
addAlias home name (Can.Alias _ _ maybeFields) graph@(Graph fieldCounts nodes) =
  case maybeFields of
    Nothing ->
      graph

    Just fields ->
      let
        function =
          Opt.Function fields $ Opt.Record $
            Map.fromList $ map toPair fields

        node =
          Opt.Define function Set.empty Set.empty
      in
      Graph
        (foldr addOne fieldCounts fields)
        (Map.insert (Opt.Global home name) node nodes)


toPair :: N.Name -> (N.Name, Opt.Expr)
toPair field =
  ( field, Opt.VarLocal field )


addOne :: N.Name -> Map.Map N.Name Int -> Map.Map N.Name Int
addOne name fields =
  Map.insertWith (+) name 1 fields



-- ADD EFFECTS


addEffects :: ModuleName.Canonical -> Can.Effects -> Graph -> Graph
addEffects home effects graph@(Graph fields nodes) =
  case effects of
    Can.NoEffects ->
      graph

    Can.Ports ports ->
      Map.foldrWithKey (addPort home) graph ports

    Can.Manager _ _ _ manager ->
      Graph fields $
        let
          fx = Opt.Global home "$fx$"
          cmd = Opt.Global home "command"
          sub = Opt.Global home "subscription"
          link = Opt.Link fx
        in
        case manager of
          Can.Cmd _ ->
            Map.insert cmd link $
            Map.insert fx (Opt.Manager Opt.Cmd) nodes

          Can.Sub _ ->
            Map.insert sub link $
            Map.insert fx (Opt.Manager Opt.Sub) nodes

          Can.Fx _ _ ->
            Map.insert cmd link $
            Map.insert sub link $
            Map.insert fx (Opt.Manager Opt.Fx) nodes



addPort :: ModuleName.Canonical -> N.Name -> Can.Port -> Graph -> Graph
addPort home name port_ graph =
  case port_ of
    Can.Incoming _ tipe ->
      let
        (kernels, globals, fields, decoder) = Names.run (Port.toDecoder tipe)
        node = Opt.PortIncoming decoder kernels globals
      in
      addToGraph (Opt.Global home name) node fields graph

    Can.Outgoing _ tipe ->
      let
        (kernels, globals, fields, encoder) = Names.run (Port.toEncoder tipe)
        node = Opt.PortOutgoing encoder kernels globals
      in
      addToGraph (Opt.Global home name) node fields graph



-- ADD DECLS


addDecls :: ModuleName.Canonical -> Can.Decls -> Graph -> Graph
addDecls home decls graph =
  case decls of
    Can.Declare def subDecls ->
      addDecls home subDecls (addDef home def graph)

    Can.DeclareRec defs subDecls ->
      addDecls home subDecls (addRecDefs home defs graph)

    Can.SaveTheEnvironment ->
      graph



-- ADD DEFS


addDef :: ModuleName.Canonical -> Can.Def -> Graph -> Graph
addDef home def graph =
  case def of
    Can.Def (A.At _ name) args body ->
      addDefHelp (Opt.Global home name) args body graph

    Can.TypedDef (A.At _ name) _ typedArgs body _ ->
      addDefHelp (Opt.Global home name) (map fst typedArgs) body graph


addDefHelp :: Opt.Global -> [Can.Pattern] -> Can.Expr -> Graph -> Graph
addDefHelp name args body graph =
  case args of
    [] ->
      let
        (kernels, globals, fields, value) =
          Names.run (Expr.optimize Set.empty body)
      in
      addToGraph name (Opt.Define value kernels globals) fields graph

    _ ->
      let
        (kernels, globals, fields, function) =
          Names.run $
            do  (argNames, defss) <- Expr.destructArgs args
                obody <- Expr.optimize Set.empty body
                pure $ Opt.Function argNames $
                  foldr Opt.Let obody (concat defss)
      in
      addToGraph name (Opt.Define function kernels globals) fields graph


addToGraph :: Opt.Global -> Opt.Node -> Map.Map N.Name Int -> Graph -> Graph
addToGraph name node fields (Graph fieldCounts nodes) =
  Graph
    (Map.unionWith (+) fields fieldCounts)
    (Map.insert name node nodes)



-- ADD RECURSIVE DEFS


addRecDefs :: ModuleName.Canonical -> [Can.Def] -> Graph -> Graph
addRecDefs home defs graph =
  let
    cycleValues =
      foldr addValueName Set.empty defs

    dummyName =
      Opt.Global home (N.toCompositeName cycleValues)

    (kernels, globals, fields, State cyclicValuePairs newGraph) =
      Names.run $
        foldM (addRecDef home dummyName cycleValues) (State [] graph) defs

    node =
      Opt.Cycle cyclicValuePairs kernels globals
  in
  addToGraph dummyName node fields newGraph


addValueName :: Can.Def -> Set.Set N.Name -> Set.Set N.Name
addValueName def valueNames =
  case def of
    Can.Def (A.At _ name) [] _ ->
      Set.insert name valueNames

    Can.TypedDef (A.At _ name) _ [] _ _ ->
      Set.insert name valueNames

    _ ->
      valueNames



-- ADD RECURSIVE DEFS


data State =
  State
    { _values :: [(N.Name, Opt.Expr)]
    , _graph :: Graph
    }


addRecDef :: ModuleName.Canonical -> Opt.Global -> Expr.Cycle -> State -> Can.Def -> Names.Tracker State
addRecDef home dummyName cycle state@(State values graph) def =
  case def of
    Can.Def (A.At _ name) [] body ->
      addRecValue home dummyName cycle name body state

    Can.TypedDef (A.At _ name) _ [] body _ ->
      addRecValue home dummyName cycle name body state

    _ ->
      let
        (kernels, globals, fields, odef) =
          Names.run (Expr.optimizePotentialTailCall Set.empty def)
      in
      pure $ State values $
        case odef of
          Opt.Def name body ->
            let node = Opt.Define body kernels globals in
            addToGraph (Opt.Global home name) node fields graph

          Opt.TailDef name args body ->
            let node = Opt.DefineTailFunc args body kernels globals in
            addToGraph (Opt.Global home name) node fields graph


addRecValue :: ModuleName.Canonical -> Opt.Global -> Expr.Cycle -> N.Name -> Can.Expr -> State -> Names.Tracker State
addRecValue home dummyName cycle name body (State values (Graph fields nodes)) =
  do  obody <- Expr.optimize cycle body
      pure $
        State
          { _values = (name, obody) : values
          , _graph = Graph fields $
              Map.insert (Opt.Global home name) (Opt.Link dummyName) nodes
          }
