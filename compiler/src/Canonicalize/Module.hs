{-# OPTIONS_GHC -Wall #-}
module Canonicalize.Module
  ( canonicalize
  )
  where


import qualified Data.Graph as Graph
import qualified Data.Map as Map

import qualified AST.Canonical as Can
import qualified AST.Source as Src
import qualified AST.Valid as Valid
import qualified AST.Module.Name as ModuleName
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
import qualified Elm.Name as N
import qualified Elm.Package as Pkg
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Canonicalize as Error
import qualified Reporting.Region as R
import qualified Reporting.Result as Result
import qualified Reporting.Warning as W



-- RESULT


type Result i w a =
  Result.Result i w Error.Error a



-- MODULES


canonicalize
  :: Pkg.Name
  -> Map.Map N.Name ModuleName.Canonical
  -> I.Interfaces
  -> Valid.Module
  -> Result i [W.Warning] Can.Module
canonicalize pkg importDict interfaces module_@(Valid.Module name _ _ exports imports decls _ _ binops effects) =
  do  let home = ModuleName.Canonical pkg name
      let cbinops = Map.fromList (map canonicalizeBinop binops)

      (env, cunions, caliases) <-
        Local.add module_ =<<
          Foreign.createInitialEnv home importDict interfaces imports

      cdecls <- canonicalizeDecls env decls
      ceffects <- Effects.canonicalize env decls cunions effects
      cexports <- canonicalizeExports decls cunions caliases cbinops ceffects exports

      return $ Can.Module home (toDocs module_) cexports cdecls cunions caliases cbinops ceffects



-- DOCUMENTATION


toDocs :: Valid.Module -> Can.Docs
toDocs (Valid.Module _ docs comments _ _ _ _ _ _ _) =
  case docs of
    Src.NoDocs region ->
      Can.NoDocs region

    Src.YesDocs region overview ->
      Can.YesDocs region overview comments



-- CANONICALIZE BINOP


canonicalizeBinop :: Valid.Binop -> ( N.Name, Can.Binop )
canonicalizeBinop (Valid.Binop op associativity precedence func) =
  ( op, Can.Binop_ associativity precedence func )



-- DECLARATIONS / CYCLE DETECTION
--
-- There are two phases of cycle detection:
--
-- 1. Detect cycles using ALL dependencies => needed for type inference
-- 2. Detect cycles using DIRECT dependencies => nonterminating recursion
--


canonicalizeDecls :: Env.Env -> [A.Located Valid.Decl] -> Result i [W.Warning] Can.Decls
canonicalizeDecls env decls =
  do  nodes <- traverse (toNodeOne env) decls
      detectCycles (Graph.stronglyConnComp nodes)


detectCycles :: [Graph.SCC NodeTwo] -> Result i w Can.Decls
detectCycles sccs =
  case sccs of
    [] ->
      Result.ok Can.SaveTheEnvironment

    scc : otherSccs ->
      case scc of
        Graph.AcyclicSCC (def, _, _) ->
          Can.Declare def <$> detectCycles otherSccs

        Graph.CyclicSCC [] ->
          detectCycles otherSccs

        Graph.CyclicSCC subNodes ->
          Can.DeclareRec
            <$> traverse detectBadCycles (Graph.stronglyConnComp subNodes)
            <*> detectCycles otherSccs


detectBadCycles :: Graph.SCC Can.Def -> Result i w Can.Def
detectBadCycles scc =
  case scc of
    Graph.AcyclicSCC def ->
      Result.ok def

    Graph.CyclicSCC cyclicValueDefs ->
      Result.throw (Error.RecursiveDecl cyclicValueDefs)



-- DECLARATIONS / CYCLE DETECTION SETUP
--
-- toNodeOne and toNodeTwo set up nodes for the two cycle detection phases.
--

-- Phase one nodes track ALL dependencies.
-- This allows us to find cyclic values for type inference.
type NodeOne =
  (NodeTwo, N.Name, [N.Name])


-- Phase two nodes track DIRECT dependencies.
-- This allows us to detect cycles that definitely do not terminate.
type NodeTwo =
  (Can.Def, N.Name, [N.Name])


toNodeOne :: Env.Env -> A.Located Valid.Decl -> Result i [W.Warning] NodeOne
toNodeOne env (A.At _ (Valid.Decl aname@(A.At _ name) srcArgs body maybeType)) =
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
          return
            ( toNodeTwo name srcArgs def freeLocals
            , name
            , Map.keys freeLocals
            )

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
          return
            ( toNodeTwo name srcArgs def freeLocals
            , name
            , Map.keys freeLocals
            )


toNodeTwo :: N.Name -> [arg] -> Can.Def -> Expr.FreeLocals -> NodeTwo
toNodeTwo name args def freeLocals =
  case args of
    [] ->
      (def, name, Map.foldrWithKey addDirects [] freeLocals)

    _ ->
      (def, name, [])


addDirects :: N.Name -> Expr.Uses -> [N.Name] -> [N.Name]
addDirects name (Expr.Uses directUses _) directDeps =
  if directUses > 0 then
    name:directDeps
  else
    directDeps



-- CANONICALIZE EXPORTS


canonicalizeExports
  :: [A.Located Valid.Decl]
  -> Map.Map N.Name union
  -> Map.Map N.Name alias
  -> Map.Map N.Name binop
  -> Can.Effects
  -> A.Located Src.Exposing
  -> Result i w Can.Exports
canonicalizeExports decls unions aliases binops effects (A.At region exposing) =
  case exposing of
    Src.Open ->
      Result.ok (Can.ExportEverything region)

    Src.Explicit exposeds ->
      do  let names = Map.fromList (map declToName decls)
          infos <- traverse (checkExposed names unions aliases binops effects) exposeds
          Can.Export <$> Dups.detect Error.ExportDuplicate (Dups.unions infos)


declToName :: A.Located Valid.Decl -> ( N.Name, () )
declToName (A.At _ (Valid.Decl (A.At _ name) _ _ _)) =
  ( name, () )


checkExposed
  :: Map.Map N.Name decl
  -> Map.Map N.Name union
  -> Map.Map N.Name alias
  -> Map.Map N.Name binop
  -> Can.Effects
  -> A.Located Src.Exposed
  -> Result i w (Dups.Dict (A.Located Can.Export))
checkExposed decls unions aliases binops effects (A.At region exposed) =
  case exposed of
    Src.Lower name ->
      if Map.member name decls then
        ok name region Can.ExportValue
      else
        case checkPorts effects name of
          Nothing ->
            ok name region Can.ExportPort

          Just ports ->
            Result.throw $ Error.ExportNotFound region Error.BadVar name $
              ports ++ Map.keys decls

    Src.Operator name ->
      if Map.member name binops then
        ok name region Can.ExportBinop
      else
        Result.throw $ Error.ExportNotFound region Error.BadOp name $
          Map.keys binops

    Src.Upper name Src.Public ->
      if Map.member name unions then
        ok name region Can.ExportUnionOpen
      else if Map.member name aliases then
        Result.throw $ Error.ExportOpenAlias region name
      else
        Result.throw $ Error.ExportNotFound region Error.BadType name $
          Map.keys unions ++ Map.keys aliases

    Src.Upper name Src.Private ->
      if Map.member name unions then
        ok name region Can.ExportUnionClosed
      else if Map.member name aliases then
        ok name region Can.ExportAlias
      else
        Result.throw $ Error.ExportNotFound region Error.BadType name $
          Map.keys unions ++ Map.keys aliases


checkPorts :: Can.Effects -> N.Name -> Maybe [N.Name]
checkPorts effects name =
  case effects of
    Can.NoEffects ->
      Just []

    Can.Ports ports ->
      if Map.member name ports then Nothing else Just (Map.keys ports)

    Can.Manager _ _ _ _ ->
      Just []


ok :: N.Name -> R.Region -> Can.Export -> Result i w (Dups.Dict (A.Located Can.Export))
ok name region export =
  Result.ok $ Dups.one name region (A.At region export)
