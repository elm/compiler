{-# OPTIONS_GHC -Wall #-}
module Canonicalize.Module
  ( canonicalize
  )
  where


import qualified Data.Graph as Graph
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified AST.Expression.Canonical as Can
import qualified AST.Expression.Valid as Valid
import qualified AST.Module.Name as ModuleName
import qualified AST.Type as Type
import qualified Canonicalize.Environment as Env
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
      let (Valid.Module name _ _ _ _ decls unions aliases binops _) = module_
      Can.Module (ModuleName.Canonical pkg name)
        <$> error "TODO docs"
        <*> error "TODO imports"
        <*> canonicalizeDecls env decls
        <*> (Map.fromList <$> traverse (canonicalizeUnion env) unions)
        <*> (Map.fromList <$> traverse (canonicalizeAlias env) aliases)
        <*> return (Map.fromList (map canonicalizeBinop binops))
        <*> error "TODO effects"



-- CANONICALIZE ALIAS


canonicalizeAlias :: Env.Env -> Valid.Alias -> Result ( N.Name, Can.Alias )
canonicalizeAlias env (Valid.Alias (A.A _ name) args tipe) =
  do  ctipe <- Type.canonicalize env tipe
      return (name, Can.Alias (map A.drop args) ctipe)



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
