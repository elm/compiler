{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Optimize.Module
  ( optimize
  )
  where


import Prelude hiding (cycle)
import Control.Monad (foldM)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map ((!))

import qualified AST.Canonical as Can
import qualified AST.Optimized as Opt
import qualified AST.Module.Name as ModuleName
import qualified AST.Utils.Type as Type
import qualified Canonicalize.Effects as Effects
import qualified Elm.Name as N
import qualified Optimize.Expression as Expr
import qualified Optimize.Names as Names
import qualified Optimize.Port as Port
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Main as E
import qualified Reporting.Region as R
import qualified Reporting.Result as Result
import qualified Reporting.Warning as W



-- OPTIMIZE


type Result i w a =
  Result.Result i w E.Error a


type Annotations =
  Map.Map N.Name Can.Annotation


optimize :: Annotations -> Can.Module -> Result i [W.Warning] Opt.Graph
optimize annotations (Can.Module home _ _ decls unions aliases _ effects) =
  addDecls home annotations decls $
    addEffects home effects $
      addUnions home unions $
        addAliases home aliases $
          Opt.Graph Map.empty Map.empty Map.empty



-- UNION


type Nodes =
  Map.Map Opt.Global Opt.Node


addUnions :: ModuleName.Canonical -> Map.Map N.Name Can.Union -> Opt.Graph -> Opt.Graph
addUnions home unions (Opt.Graph mains nodes fields) =
  Opt.Graph mains (Map.foldr (addUnion home) nodes unions) fields


addUnion :: ModuleName.Canonical -> Can.Union -> Nodes -> Nodes
addUnion home (Can.Union _ ctors _ opts) nodes =
  List.foldl' (addCtorNode home opts) nodes ctors


addCtorNode :: ModuleName.Canonical -> Can.CtorOpts -> Nodes -> Can.Ctor -> Nodes
addCtorNode home opts nodes (Can.Ctor name index numArgs _) =
  let
    node =
      case opts of
        Can.Normal -> Opt.Ctor name index numArgs
        Can.Unbox -> Opt.Box name
        Can.Enum -> Opt.Enum name index
  in
  Map.insert (Opt.Global home name) node nodes



-- ALIAS


addAliases :: ModuleName.Canonical -> Map.Map N.Name Can.Alias -> Opt.Graph -> Opt.Graph
addAliases home aliases graph =
  Map.foldrWithKey (addAlias home) graph aliases


addAlias :: ModuleName.Canonical -> N.Name -> Can.Alias -> Opt.Graph -> Opt.Graph
addAlias home name (Can.Alias _ _ maybeFields) graph@(Opt.Graph mains nodes fieldCounts) =
  case maybeFields of
    Nothing ->
      graph

    Just fields ->
      let
        function =
          Opt.Function (map fst fields) $ Opt.Record $
            Map.fromList $ map toPair fields

        node =
          Opt.Define function Set.empty
      in
      Opt.Graph
        mains
        (Map.insert (Opt.Global home name) node nodes)
        (foldr addRecordCtorField fieldCounts fields)


toPair :: (N.Name, Can.Type) -> (N.Name, Opt.Expr)
toPair (field, _) =
  ( field, Opt.VarLocal field )


addRecordCtorField :: (N.Name, Can.Type) -> Map.Map N.Name Int -> Map.Map N.Name Int
addRecordCtorField (name, _) fields =
  Map.insertWith (+) name 1 fields



-- ADD EFFECTS


addEffects :: ModuleName.Canonical -> Can.Effects -> Opt.Graph -> Opt.Graph
addEffects home effects graph@(Opt.Graph mains nodes fields) =
  case effects of
    Can.NoEffects ->
      graph

    Can.Ports ports ->
      Map.foldrWithKey (addPort home) graph ports

    Can.Manager _ _ _ manager ->
      let
        fx = Opt.Global home "$fx$"
        cmd = Opt.Global home "command"
        sub = Opt.Global home "subscription"
        link = Opt.Link fx
        newNodes =
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
      in
      Opt.Graph mains newNodes fields


addPort :: ModuleName.Canonical -> N.Name -> Can.Port -> Opt.Graph -> Opt.Graph
addPort home name port_ graph =
  case port_ of
    Can.Incoming _ payloadType _ ->
      let
        (deps, fields, decoder) = Names.run (Port.toDecoder payloadType)
        node = Opt.PortIncoming decoder deps
      in
      addToGraph (Opt.Global home name) node fields graph

    Can.Outgoing _ payloadType _ ->
      let
        (deps, fields, encoder) = Names.run (Port.toEncoder payloadType)
        node = Opt.PortOutgoing encoder deps
      in
      addToGraph (Opt.Global home name) node fields graph



-- HELPER


addToGraph :: Opt.Global -> Opt.Node -> Map.Map N.Name Int -> Opt.Graph -> Opt.Graph
addToGraph name node fields (Opt.Graph mains nodes fieldCounts) =
  Opt.Graph
    mains
    (Map.insert name node nodes)
    (Map.unionWith (+) fields fieldCounts)



-- ADD DECLS


addDecls :: ModuleName.Canonical -> Annotations -> Can.Decls -> Opt.Graph -> Result i [W.Warning] Opt.Graph
addDecls home annotations decls graph =
  case decls of
    Can.Declare def subDecls ->
      addDecls home annotations subDecls =<< addDef home annotations def graph

    Can.DeclareRec defs subDecls ->
      case findMain defs of
        Nothing ->
          addDecls home annotations subDecls (addRecDefs home defs graph)

        Just region ->
          Result.throw $ E.BadCycle region (map defToName defs)

    Can.SaveTheEnvironment ->
      Result.ok graph


findMain :: [Can.Def] -> Maybe R.Region
findMain defs =
  case defs of
    [] ->
      Nothing

    def:rest ->
      case def of
        Can.Def (A.At region name) _ _ ->
          if name == N.main then Just region else findMain rest

        Can.TypedDef (A.At region name) _ _ _ _ ->
          if name == N.main then Just region else findMain rest


defToName :: Can.Def -> N.Name
defToName def =
  case def of
    Can.Def (A.At _ name) _ _          -> name
    Can.TypedDef (A.At _ name) _ _ _ _ -> name



-- ADD DEFS


addDef :: ModuleName.Canonical -> Annotations -> Can.Def -> Opt.Graph -> Result i [W.Warning] Opt.Graph
addDef home annotations def graph =
  case def of
    Can.Def (A.At region name) args body ->
      do  let (Can.Forall _ tipe) = annotations ! name
          Result.warn $ W.MissingTypeAnnotation region name tipe
          addDefHelp region annotations home name args body graph

    Can.TypedDef (A.At region name) _ typedArgs body _ ->
      addDefHelp region annotations home name (map fst typedArgs) body graph


addDefHelp :: R.Region -> Annotations -> ModuleName.Canonical -> N.Name -> [Can.Pattern] -> Can.Expr -> Opt.Graph -> Result i w Opt.Graph
addDefHelp region annotations home name args body graph@(Opt.Graph mains nodes fieldCounts) =
  if name /= N.main then
    Result.ok (addDefNode home name args body Set.empty graph)
  else
    let
      (Can.Forall _ tipe) = annotations ! name

      addMain (deps, fields, main) =
        let
          newMains = Map.insert home main mains
          newFields = Map.unionWith (+) fields fieldCounts
        in
        addDefNode home name args body deps $
          Opt.Graph newMains nodes newFields
    in
    case Type.deepDealias tipe of
      Can.TType hm nm [_] | hm == ModuleName.virtualDom && nm == N.node ->
          Result.ok $ addMain $ Names.run $
            Names.registerKernel N.browser Opt.Static

      Can.TType hm nm [flags, message, _] | hm == ModuleName.platform && nm == N.program ->
          case Effects.checkPayload flags of
            Right () ->
              Result.ok $ addMain $ Names.run $
                Opt.Dynamic message <$> Port.toFlagsDecoder flags

            Left (subType, invalidPayload) ->
              Result.throw (E.BadFlags region subType invalidPayload)

      _ ->
          Result.throw (E.BadType region tipe)


addDefNode :: ModuleName.Canonical -> N.Name -> [Can.Pattern] -> Can.Expr -> Set.Set Opt.Global -> Opt.Graph -> Opt.Graph
addDefNode home name args body mainDeps graph =
  let
    (deps, fields, def) =
      Names.run $
        case args of
          [] ->
            Expr.optimize Set.empty body

          _ ->
            do  (argNames, destructors) <- Expr.destructArgs args
                obody <- Expr.optimize Set.empty body
                pure $ Opt.Function argNames $
                  foldr Opt.Destruct obody destructors
  in
  addToGraph (Opt.Global home name) (Opt.Define def (Set.union deps mainDeps)) fields graph



-- ADD RECURSIVE DEFS


data State =
  State
    { _values :: [(N.Name, Opt.Expr)]
    , _graph :: Opt.Graph
    }


addRecDefs :: ModuleName.Canonical -> [Can.Def] -> Opt.Graph -> Opt.Graph
addRecDefs home defs graph =
  let
    cycleValues = foldr addValueName Set.empty defs
    dummyName = Opt.Global home (N.toCompositeName (Set.toList cycleValues))

    (deps, fields, State cyclicValuePairs newGraph) =
      Names.run $
        foldM (addRecDef home dummyName cycleValues) (State [] graph) defs
  in
  addToGraph dummyName (Opt.Cycle cyclicValuePairs deps) fields newGraph


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


addRecDef :: ModuleName.Canonical -> Opt.Global -> Expr.Cycle -> State -> Can.Def -> Names.Tracker State
addRecDef home dummyName cycle state@(State values graph) def =
  case def of
    Can.Def (A.At _ name) [] body ->
      addRecValue home dummyName cycle name body state

    Can.TypedDef (A.At _ name) _ [] body _ ->
      addRecValue home dummyName cycle name body state

    _ ->
      let
        (deps, fields, odef) =
          Names.run (Expr.optimizePotentialTailCall cycle def)
      in
      pure $ State values $
        case odef of
          Opt.Def name body ->
            let node = Opt.Define body deps in
            addToGraph (Opt.Global home name) node fields graph

          Opt.TailDef name args body ->
            let node = Opt.DefineTailFunc args body deps in
            addToGraph (Opt.Global home name) node fields graph


addRecValue :: ModuleName.Canonical -> Opt.Global -> Expr.Cycle -> N.Name -> Can.Expr -> State -> Names.Tracker State
addRecValue home dummyName cycle name body (State values (Opt.Graph mains nodes fields)) =
  do  obody <- Expr.optimize cycle body

      let global = Opt.Global home name
      let node = Opt.Link dummyName

      pure $
        State
          { _values = (name, obody) : values
          , _graph = Opt.Graph mains (Map.insert global node nodes) fields
          }
