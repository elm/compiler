{-# OPTIONS_GHC -Wall #-}
module Canonicalize.Module
  ( canonicalize
  )
  where


import qualified Data.Graph as Graph
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified AST.Expression.Canonical as Can
import qualified AST.Expression.Source as Src
import qualified AST.Expression.Valid as Valid
import qualified AST.Module.Name as ModuleName
import qualified AST.Type as Type
import qualified Canonicalize.Environment as Env
import qualified Canonicalize.Environment.Dups as Dups
import qualified Canonicalize.Expression as Expr
import qualified Canonicalize.Pattern as Pattern
import qualified Canonicalize.Type as Type
import qualified Elm.Interface as I
import qualified Elm.Name as N
import qualified Elm.Package as Pkg
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Canonicalize as Error
import qualified Reporting.Region as R
import qualified Reporting.Result as Result
import qualified Reporting.Warning as Warning



-- RESULT


type Result a =
  Result.Result () Warning.Warning Error.Error a



-- MODULES


canonicalize
  :: Pkg.Name
  -> Map.Map ModuleName.Raw ModuleName.Canonical
  -> I.Interfaces
  -> Valid.Module
  -> Result Can.Module
canonicalize pkg importDict interfaces module_ =
  do  env <- Env.create pkg importDict interfaces module_
      let (Valid.Module name _ _ exports _ decls unions aliases binops effects) = module_
      (cdecls, cunions, caliases) <-
        (,,)
          <$> canonicalizeDecls env decls
          <*> (Map.fromList <$> traverse (canonicalizeUnion env) unions)
          <*> (Map.fromList <$> traverse (canonicalizeAlias env) aliases)

      let cbinops = Map.fromList (map canonicalizeBinop binops)
      ceffects <- canonicalizeEffects env cunions effects
      cexports <- canonicalizeExports decls cunions caliases cbinops ceffects exports

      let home = ModuleName.Canonical pkg name
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


toRecordCtorInfo :: Type.Raw -> Maybe [N.Name]
toRecordCtorInfo (A.A _ tipe) =
  case tipe of
    Type.RRecord fields Nothing ->
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


detectCycles :: [Graph.SCC Decl] -> Result Can.Decls
detectCycles sccs =
  case sccs of
    [] ->
      Result.ok Can.SaveTheEnvironment

    scc : otherSccs ->
      case scc of
        Graph.AcyclicSCC decl ->
          Can.Declare (toCanDecl decl)
            <$> detectCycles otherSccs

        Graph.CyclicSCC [] ->
          detectCycles otherSccs

        Graph.CyclicSCC (decl:decls) ->
          do  detectBadCycles decl decls
              Can.DeclareRec (toCanDecl decl) (map toCanDecl decls)
                <$> detectCycles otherSccs


toNode :: Env.Env -> A.Located Valid.Decl -> Result (Decl, N.Name, [N.Name])
toNode env (A.A region (Valid.Decl name@(A.A _ key) args body tipe)) =
  case args of
    [] ->
      do  ctipe <- traverse (Type.canonicalize env) tipe
          (cbody, freeLocals) <- Expr.removeLocals Map.empty $ Expr.canonicalize env body
          return (Value region name cbody ctipe, key, Set.toList freeLocals)

    _ ->
      do  ctipe <- traverse (Type.canonicalize env) tipe
          cargs@(Can.Args _ destructors) <- Pattern.canonicalizeArgs env args
          newEnv <- Env.addLocals destructors env
          (cbody, freeLocals) <- Expr.removeLocals destructors $ Expr.canonicalize newEnv body
          return (Function key cargs cbody ctipe, key, Set.toList freeLocals)


data Decl
  = Value R.Region (A.Located N.Name) Can.Expr (Maybe Type.Canonical)
  | Function N.Name Can.Args Can.Expr (Maybe Type.Canonical)


toCanDecl :: Decl -> Can.Decl
toCanDecl decl =
  case decl of
    Value _ (A.A _ name) body tipe ->
      Can.Value name body tipe

    Function name args body tipe ->
      Can.Function name args body tipe


detectBadCycles :: Decl -> [Decl] -> Result ()
detectBadCycles decl decls =
  case traverse detectBadCyclesHelp (decl:decls) of
    Nothing ->
      Result.ok ()

    Just valueCycle ->
      case valueCycle of
        [] ->
          Result.ok ()

        (declRegion, A.A region name) : otherInfo ->
          let otherNames = map (A.drop . snd) otherInfo in
          Result.throw declRegion (Error.RecursiveDecl region name otherNames)


detectBadCyclesHelp :: Decl -> Maybe (R.Region, A.Located N.Name)
detectBadCyclesHelp decl =
  case decl of
    Value region name _ _ ->
      Just (region, name)

    Function _ _ _ _ ->
      Nothing



-- CANONICALIZE EFFECTS


canonicalizeEffects :: Env.Env -> Map.Map N.Name a -> Valid.Effects -> Result Can.Effects
canonicalizeEffects env unions effects =
  case effects of
    Valid.NoEffects ->
      Result.ok Can.NoEffects

    Valid.Ports ports ->
      do  pairs <- traverse (canonicalizePort env) ports
          return $ Can.Ports (Map.fromList pairs)

    Valid.Cmd cmdType ->
      Can.Cmd <$> verifyEffectType cmdType unions

    Valid.Sub subType ->
      Can.Sub <$> verifyEffectType subType unions

    Valid.Fx cmdType subType ->
      Can.Fx
        <$> verifyEffectType cmdType unions
        <*> verifyEffectType subType unions


canonicalizePort :: Env.Env -> Valid.Port -> Result (N.Name, Type.Canonical)
canonicalizePort env (Valid.Port (A.A _ name) tipe) =
  (,) name <$> Type.canonicalize env tipe


verifyEffectType :: A.Located N.Name -> Map.Map N.Name a -> Result N.Name
verifyEffectType (A.A region name) unions =
  if Map.member name unions then
    Result.ok name
  else
    Result.throw region (Error.EffectNotFound name)



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
          Can.Export <$> Dups.detect toError infos


declToName :: A.Located Valid.Decl -> (N.Name, Can.Export)
declToName (A.A _ (Valid.Decl (A.A _ name) _ _ _)) =
  (name, Can.ExportValue)


checkExposed
  :: Map.Map N.Name decl
  -> Map.Map N.Name union
  -> Map.Map N.Name alias
  -> Map.Map N.Name binop
  -> Can.Effects
  -> A.Located Src.Exposed
  -> Result (N.Name, [Dups.Info () Can.Export])
checkExposed decls unions aliases binops effects (A.A region exposed) =
  case exposed of
    Src.Lower name ->
      if Map.member name decls then
        Result.ok (Dups.info name region () Can.ExportValue)
      else
        case checkPorts effects name of
          Nothing ->
            Result.ok (Dups.info name region () Can.ExportPort)

          Just ports ->
            Result.throw region $ Error.ExportNotFound Error.BadVar name $
              ports ++ Map.keys decls

    Src.Operator name ->
      if Map.member name binops then
        Result.ok (Dups.info name region () Can.ExportBinop)
      else
        Result.throw region $ Error.ExportNotFound Error.BadOp name $
          Map.keys binops

    Src.Upper name Src.Public ->
      if Map.member name unions then
        Result.ok (Dups.info name region () Can.ExportUnionOpen)
      else if Map.member name aliases then
        Result.throw region $ Error.ExportOpenAlias name
      else
        Result.throw region $ Error.ExportNotFound Error.BadType name $
          Map.keys unions ++ Map.keys aliases

    Src.Upper name Src.Private ->
      if Map.member name unions then
        Result.ok (Dups.info name region () Can.ExportUnionClosed)
      else if Map.member name aliases then
        Result.ok (Dups.info name region () Can.ExportAlias)
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

    Can.Cmd _ ->
      Just []

    Can.Sub _ ->
      Just []

    Can.Fx _ _ ->
      Just []
