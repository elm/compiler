{-# OPTIONS_GHC -Wall #-}
module Canonicalize.Module
  ( canonicalize
  )
  where


import qualified Data.Graph as Graph
import qualified Data.Map as Map
import qualified Data.Set as Set

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
import qualified Reporting.Result as Result
import qualified Reporting.Warning as Warning



-- RESULT


type Result a =
  Result.Result () Warning.Warning Error.Error a



-- MODULES


canonicalize
  :: Pkg.Name
  -> Map.Map N.Name ModuleName.Canonical
  -> I.Interfaces
  -> Valid.Module
  -> Result Can.Module
canonicalize pkg importDict interfaces module_@(Valid.Module name _ _ exports imports decls unions aliases binops effects) =
  do  let home = ModuleName.Canonical pkg name

      startEnv <- Local.addVarsAndTypes module_ =<<
        Foreign.createInitialEnv home importDict interfaces imports

      (cunions, caliases) <-
        (,)
          <$> (Map.fromList <$> traverse (canonicalizeUnion startEnv) unions)
          <*> (Map.fromList <$> traverse (canonicalizeAlias startEnv) aliases)

      let finalEnv = Local.addPatterns cunions startEnv
      let cbinops = Map.fromList (map canonicalizeBinop binops)

      (cdecls, ceffects) <-
        (,)
          <$> canonicalizeDecls finalEnv decls
          <*> Effects.canonicalize finalEnv decls cunions effects

      cexports <- canonicalizeExports decls cunions caliases cbinops ceffects exports

      let docs = toDocs module_
      return $ Can.Module home docs cexports cdecls cunions caliases cbinops ceffects



-- DOCUMENTATION


toDocs :: Valid.Module -> A.Located (Maybe Can.Docs)
toDocs (Valid.Module _ (A.A region maybeOverview) comments _ _ _ _ _ _ _) =
  case maybeOverview of
    Nothing ->
      A.A region Nothing

    Just overview ->
      A.A region (Just (Can.Docs overview comments))



-- CANONICALIZE ALIAS


canonicalizeAlias :: Env.Env -> Valid.Alias -> Result ( N.Name, Can.Alias )
canonicalizeAlias env (Valid.Alias (A.A _ name) args tipe) =
  do  ctipe <- Type.canonicalize env tipe
      return (name, Can.Alias (map A.drop args) ctipe (toRecordCtorInfo tipe))


toRecordCtorInfo :: Src.Type -> Maybe [N.Name]
toRecordCtorInfo (A.A _ tipe) =
  case tipe of
    Src.TRecord fields Nothing ->
      Just (map (A.drop . fst) fields)

    _ ->
      Nothing



-- CANONICALIZE UNION


canonicalizeUnion :: Env.Env -> Valid.Union -> Result ( N.Name, Can.Union )
canonicalizeUnion env (Valid.Union (A.A _ name) args ctors) =
  let
    canonicalizeCtor (A.A _ ctor, tipes) =
      do  ctipes <- traverse (Type.canonicalize env) tipes
          return (ctor, ctipes)
  in
  do  cctors <- traverse canonicalizeCtor ctors
      return ( name, Can.Union (map A.drop args) cctors )



-- CANONICALIZE BINOP


canonicalizeBinop :: Valid.Binop -> ( N.Name, Can.Binop )
canonicalizeBinop (Valid.Binop (A.A _ op) associativity precedence func) =
  ( op, Can.Binop_ associativity precedence func )



-- CANONICALIZE DECLARATIONS


canonicalizeDecls :: Env.Env -> [A.Located Valid.Decl] -> Result Can.Decls
canonicalizeDecls env decls =
  do  nodes <- traverse (toNode env) decls
      detectCycles (Graph.stronglyConnComp nodes)


detectCycles :: [Graph.SCC Can.Def] -> Result Can.Decls
detectCycles sccs =
  case sccs of
    [] ->
      Result.ok Can.SaveTheEnvironment

    scc : otherSccs ->
      case scc of
        Graph.AcyclicSCC def ->
          Can.Declare def <$> detectCycles otherSccs

        Graph.CyclicSCC [] ->
          detectCycles otherSccs

        Graph.CyclicSCC (def:defs) ->
          do  detectBadCycles def defs
              Can.DeclareRec def defs <$> detectCycles otherSccs


toNode :: Env.Env -> A.Located Valid.Decl -> Result (Can.Def, N.Name, [N.Name])
toNode env (A.A _ (Valid.Decl aname@(A.A _ name) srcArgs body maybeType)) =
  case maybeType of
    Nothing ->
      do  (args, destructors) <-
            Pattern.verify (Error.DPFuncArgs name) $
              Index.indexedTraverse (Pattern.canonicalizeArg env) srcArgs

          newEnv <- Env.addLocals destructors env
          (cbody, freeLocals) <-
            Expr.removeLocals Warning.Pattern destructors $
              Expr.canonicalize newEnv body

          let def = Can.Def aname args destructors cbody
          return (def, name, Set.toList freeLocals)

    Just srcType ->
      do  (Can.Forall freeVars tipe) <- Type.toAnnotation env srcType

          ((args,resultType), destructors) <-
            Pattern.verify (Error.DPFuncArgs name) $
              Expr.gatherTypedArgs env name srcArgs tipe Index.first []

          newEnv <- Env.addLocals destructors env
          (cbody, freeLocals) <-
            Expr.removeLocals Warning.Pattern destructors $
              Expr.canonicalize newEnv body

          let def = Can.TypedDef aname freeVars args destructors cbody resultType
          return (def, name, Set.toList freeLocals)


detectBadCycles :: Can.Def -> [Can.Def] -> Result ()
detectBadCycles def defs =
  case (,) <$> detectBadCyclesHelp def <*> traverse detectBadCyclesHelp defs of
    Nothing ->
      Result.ok ()

    Just (name, otherNames) ->
      Result.throw (error "TODO") (Error.RecursiveDecl name otherNames)


detectBadCyclesHelp :: Can.Def -> Maybe N.Name
detectBadCyclesHelp def =
  case def of
    Can.Def (A.A _ name) [] _ _ ->
      Just name

    Can.TypedDef (A.A _ name) _ [] _ _ _ ->
      Just name

    _ ->
      Nothing



-- CANONICALIZE EXPORTS


canonicalizeExports
  :: [A.Located Valid.Decl]
  -> Map.Map N.Name union
  -> Map.Map N.Name alias
  -> Map.Map N.Name binop
  -> Can.Effects
  -> Src.Exposing
  -> Result Can.Exports
canonicalizeExports decls unions aliases binops effects exposing =
  case exposing of
    Src.Open ->
      Result.ok Can.ExportEverything

    Src.Explicit exposeds ->
      do  let names = Map.fromList (map declToName decls)
          infos <- traverse (checkExposed names unions aliases binops effects) exposeds
          let toError name () () = Error.ExportDuplicate name
          Can.Export <$> Dups.detect toError (Dups.unions infos)


declToName :: A.Located Valid.Decl -> ( N.Name, () )
declToName (A.A _ (Valid.Decl (A.A _ name) _ _ _)) =
  ( name, () )


checkExposed
  :: Map.Map N.Name decl
  -> Map.Map N.Name union
  -> Map.Map N.Name alias
  -> Map.Map N.Name binop
  -> Can.Effects
  -> A.Located Src.Exposed
  -> Result (Dups.Dict () Can.Export)
checkExposed decls unions aliases binops effects (A.A region exposed) =
  case exposed of
    Src.Lower name ->
      if Map.member name decls then
        Result.ok (Dups.one name region () Can.ExportValue)
      else
        case checkPorts effects name of
          Nothing ->
            Result.ok (Dups.one name region () Can.ExportPort)

          Just ports ->
            Result.throw region $ Error.ExportNotFound Error.BadVar name $
              ports ++ Map.keys decls

    Src.Operator name ->
      if Map.member name binops then
        Result.ok (Dups.one name region () Can.ExportBinop)
      else
        Result.throw region $ Error.ExportNotFound Error.BadOp name $
          Map.keys binops

    Src.Upper name Src.Public ->
      if Map.member name unions then
        Result.ok (Dups.one name region () Can.ExportUnionOpen)
      else if Map.member name aliases then
        Result.throw region $ Error.ExportOpenAlias name
      else
        Result.throw region $ Error.ExportNotFound Error.BadType name $
          Map.keys unions ++ Map.keys aliases

    Src.Upper name Src.Private ->
      if Map.member name unions then
        Result.ok (Dups.one name region () Can.ExportUnionClosed)
      else if Map.member name aliases then
        Result.ok (Dups.one name region () Can.ExportAlias)
      else
        Result.throw region $ Error.ExportNotFound Error.BadType name $
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
