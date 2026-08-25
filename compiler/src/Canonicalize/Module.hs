{-# LANGUAGE TemplateHaskell #-}
module Canonicalize.Module
  ( canonicalize
  )
  where


import Prelude hiding (cycle)
import qualified Data.Map as Map
import qualified Data.Name as Name

import qualified Graph

import qualified AST.Canonical as Can
import qualified AST.Source as Src
import qualified Canonicalize.Effects as Effects
import qualified Canonicalize.Environment as Env
import qualified Canonicalize.Environment.Dups as Dups
import qualified Canonicalize.Environment.Foreign as Foreign
import qualified Canonicalize.Environment.Local as Local
import qualified Canonicalize.Expression as Expr
import qualified Canonicalize.Pattern as Pattern
import qualified Canonicalize.Type as Type
import qualified Data.Index as Index
import qualified Elm.Interface as I
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Canonicalize as Error
import qualified Reporting.Result as Result
import qualified Reporting.Warning as W



-- RESULT


type Result i w a =
  Result.Result i w Error.Error a



-- MODULES


canonicalize :: Pkg.Name -> Map.Map ModuleName.Raw I.Interface -> Src.Module -> Result i [W.Warning] Can.Module
canonicalize pkg ifaces modul@(Src.Module _ exports docs imports values _ _ binops effects) =
  do  let home = ModuleName.Canonical pkg (Src.getName modul)
      let cbinops = Map.fromList (map canonicalizeBinop binops)

      (env, cunions, caliases) <-
        Local.add modul =<<
          Foreign.createInitialEnv home ifaces imports

      cvalues <- canonicalizeValues env values
      ceffects <- Effects.canonicalize env values cunions effects
      cexports <- canonicalizeExports values cunions caliases cbinops ceffects exports

      return $ Can.Module home cexports docs cvalues cunions caliases cbinops ceffects



-- CANONICALIZE BINOP


canonicalizeBinop :: A.Located Src.Infix -> ( Name.Name, Can.Binop )
canonicalizeBinop (A.At _ (Src.Infix op associativity precedence func)) =
  ( op, Can.Binop_ associativity precedence func )



-- DECLARATIONS / CYCLE DETECTION
--
-- There are two phases of cycle detection:
--
-- 1. Detect cycles using ALL dependencies => needed for type inference
-- 2. Detect cycles using DIRECT dependencies => nonterminating recursion
--


canonicalizeValues :: Env.Env -> [A.Located Src.Value] -> Result i [W.Warning] Can.Decls
canonicalizeValues env values =
  do  nodes <- traverse (toNodeOne env) values
      detectCycles (Graph.toSCC nodes)


detectCycles :: [Graph.SCC Name.Name NodeTwo] -> Result i w Can.Decls
detectCycles sccs =
  case sccs of
    [] ->
      Result.ok Can.SaveTheEnvironment

    scc : otherSccs ->
      case scc of
        Graph.Acyclic (Graph.Node _ def _) ->
          Can.Declare (Graph._value def) <$> detectCycles otherSccs

        Graph.Cyclic component ->
          do  defs <- traverse detectBadCycles $ Graph.toSCC $ map Graph._value $ Graph.withComponent component (:)
              case defs of
                []   -> detectCycles otherSccs
                d:ds -> Can.DeclareRec d ds <$> detectCycles otherSccs


detectBadCycles :: Graph.SCC Name.Name Can.Def -> Result i w Can.Def
detectBadCycles scc =
  case scc of
    Graph.Acyclic (Graph.Node _ def _) ->
      Result.ok def

    Graph.Cyclic component ->
      Graph.withMinimalCycle selector component Graph._key $ \(A.At r n) cycle ->
        Result.throw $ Error.RecursiveDecl r n cycle
  where
    selector =
      Graph.RootSelector $ \(Graph.Node key def _) _ -> (key, getName def)

    getName def =
      case def of
        Can.Def      n _ _     -> n
        Can.TypedDef n _ _ _ _ -> n



-- DECLARATIONS / CYCLE DETECTION SETUP
--
-- toNodeOne and toNodeTwo set up nodes for the two cycle detection phases.
--

-- Phase one nodes track ALL dependencies.
-- This allows us to find cyclic values for type inference.
type NodeOne = Graph.Node Name.Name NodeTwo


-- Phase two nodes track DIRECT dependencies.
-- This allows us to detect cycles that definitely do not terminate.
type NodeTwo = Graph.Node Name.Name Can.Def


toNodeOne :: Env.Env -> A.Located Src.Value -> Result i [W.Warning] NodeOne
toNodeOne env (A.At _ (Src.Value aname@(A.At _ name) srcArgs body maybeType)) =
  case maybeType of
    Nothing ->
      do  (args, argBindings) <-
            Pattern.verify (Error.DPFuncArgs name) $
              traverse (Pattern.canonicalize env) srcArgs

          newEnv <-
            Env.addLocals argBindings env

          (cbody, freeLocals) <-
            Expr.verifyBindings W.Pattern argBindings (Expr.canonicalize newEnv body)

          let def = Can.Def aname args cbody
          return $ Graph.Node name (toNodeTwo name srcArgs def freeLocals) (Map.keys freeLocals)

    Just srcType ->
      do  (Can.Forall freeVars tipe) <- Type.toAnnotation env srcType

          ((args,resultType), argBindings) <-
            Pattern.verify (Error.DPFuncArgs name) $
              Expr.gatherTypedArgs env name srcArgs tipe Index.first []

          newEnv <-
            Env.addLocals argBindings env

          (cbody, freeLocals) <-
            Expr.verifyBindings W.Pattern argBindings (Expr.canonicalize newEnv body)

          let def = Can.TypedDef aname freeVars args cbody resultType
          return $ Graph.Node name (toNodeTwo name srcArgs def freeLocals) (Map.keys freeLocals)


toNodeTwo :: Name.Name -> [arg] -> Can.Def -> Expr.FreeLocals -> NodeTwo
toNodeTwo name args def freeLocals =
  Graph.Node name def $
    case args of
      [] -> Map.foldrWithKey addDirects [] freeLocals
      _  -> []


addDirects :: Name.Name -> Expr.Uses -> [Name.Name] -> [Name.Name]
addDirects name (Expr.Uses directUses _) directDeps =
  if directUses > 0 then
    name:directDeps
  else
    directDeps



-- CANONICALIZE EXPORTS


canonicalizeExports
  :: [A.Located Src.Value]
  -> Map.Map Name.Name union
  -> Map.Map Name.Name alias
  -> Map.Map Name.Name binop
  -> Can.Effects
  -> A.Located Src.Exposing
  -> Result i w Can.Exports
canonicalizeExports values unions aliases binops effects (A.At region exposing) =
  case exposing of
    Src.Open ->
      Result.ok (Can.ExportEverything region)

    Src.Explicit exposeds ->
      do  let names = Map.fromList (map valueToName values)
          infos <- traverse (checkExposed names unions aliases binops effects) exposeds
          Can.Export <$> Dups.detect Error.ExportDuplicate (Dups.unions infos)


valueToName :: A.Located Src.Value -> ( Name.Name, () )
valueToName (A.At _ (Src.Value (A.At _ name) _ _ _)) =
  ( name, () )


checkExposed
  :: Map.Map Name.Name value
  -> Map.Map Name.Name union
  -> Map.Map Name.Name alias
  -> Map.Map Name.Name binop
  -> Can.Effects
  -> Src.Exposed
  -> Result i w (Dups.Dict (A.Located Can.Export))
checkExposed values unions aliases binops effects exposed =
  case exposed of
    Src.Lower (A.At region name) ->
      if Map.member name values then
        ok name region Can.ExportValue
      else
        case checkPorts effects name of
          Nothing ->
            ok name region Can.ExportPort

          Just ports ->
            Result.throw $ Error.ExportNotFound region Error.BadVar name $
              ports ++ Map.keys values

    Src.Operator region name ->
      if Map.member name binops then
        ok name region Can.ExportBinop
      else
        Result.throw $ Error.ExportNotFound region Error.BadOp name $
          Map.keys binops

    Src.Upper (A.At region name) (Src.Public dotDotRegion) ->
      if Map.member name unions then
        ok name region Can.ExportUnionOpen
      else if Map.member name aliases then
        Result.throw $ Error.ExportOpenAlias dotDotRegion name
      else
        Result.throw $ Error.ExportNotFound region Error.BadType name $
          Map.keys unions ++ Map.keys aliases

    Src.Upper (A.At region name) Src.Private ->
      if Map.member name unions then
        ok name region Can.ExportUnionClosed
      else if Map.member name aliases then
        ok name region Can.ExportAlias
      else
        Result.throw $ Error.ExportNotFound region Error.BadType name $
          Map.keys unions ++ Map.keys aliases


checkPorts :: Can.Effects -> Name.Name -> Maybe [Name.Name]
checkPorts effects name =
  case effects of
    Can.NoEffects ->
      Just []

    Can.Ports ports ->
      if Map.member name ports then Nothing else Just (Map.keys ports)

    Can.Manager _ _ _ _ ->
      Just []


ok :: Name.Name -> A.Region -> Can.Export -> Result i w (Dups.Dict (A.Located Can.Export))
ok name region export =
  Result.ok $ Dups.one name region (A.At region export)
