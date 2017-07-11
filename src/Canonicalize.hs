{-# OPTIONS_GHC -Wall #-}
module Canonicalize (module') where

import Prelude hiding (last)
import qualified Data.Foldable as T
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Data.Text (Text)

import qualified AST.Declaration as D
import qualified AST.Effects as Effects
import qualified AST.Exposing as Exposing
import qualified AST.Expression.Source as Src
import qualified AST.Expression.Canonical as C
import qualified AST.Module as Module
import qualified AST.Module.Name as ModuleName
import qualified AST.Pattern as P
import qualified AST.Type as Type
import qualified AST.Variable as Var
import qualified Canonicalize.Binops as Binops
import qualified Canonicalize.Body as Body
import qualified Canonicalize.Effects as Effects
import qualified Canonicalize.Environment as Env
import qualified Canonicalize.Setup as Setup
import qualified Canonicalize.Type as Canon
import qualified Canonicalize.Variable as Canon
import qualified Docs.Centralize as Docs
import qualified Reporting.Annotation as A
import qualified Reporting.Error as Error
import qualified Reporting.Error.Canonicalize as CError
import qualified Reporting.Helpers as Help
import qualified Reporting.Region as Region
import qualified Reporting.Render.Type as RenderType
import qualified Reporting.Result as Result
import qualified Reporting.Warning as Warning



type Result a =
  Result.Result (Result.One RenderType.Localizer) Warning.Warning Error.Error a


-- MODULES


module'
    :: Map.Map ModuleName.Raw ModuleName.Canonical
    -> Module.Interfaces
    -> Module.Valid
    -> Result Module.Canonical
module' importDict interfaces modul =
  let
    (Module.Valid docs exports imports decls effects) =
      Module.info modul

    (Result.Result uses warnings rawResults) =
      do  env <- Setup.environment importDict interfaces modul

          (,,,) env
            <$> resolveExports (getInternalApi effects decls) exports
            <*> canonicalizeDecls env decls
            <*> Effects.canonicalize env effects
  in
    case rawResults of
      Result.Err errors ->
        Result.mapError Error.Canonicalize $
          Result.Result Result.None warnings (Result.Err errors)

      Result.Ok (env, canonicalExports, canonicalDecls, canonicalEffects) ->
        let
          (D.Decls _ unions aliases infixes) =
            canonicalDecls

          centralizedDocs =
            A.map (fmap (Docs.centralize canonicalDecls)) docs

          typeToPair (A.A _ (D.Type name args body)) =
            ( name, (args, body) )

          getProgram =
            Body.flatten (Module.name modul) canonicalDecls canonicalEffects
        in
          do  program <- Result.mapError Error.Canonicalize getProgram
              canonicalImports <- filterImports uses imports
              Result.accumulate (Result.One (Env.toLocalizer env)) $
                modul {
                  Module.info =
                    Module.Info
                      { Module.docs = centralizedDocs
                      , Module.exports = canonicalExports
                      , Module.imports = canonicalImports
                      , Module.program = program
                      , Module.types = Map.empty
                      , Module.fixities = infixes
                      , Module.aliases = Map.fromList (map typeToPair aliases)
                      , Module.unions = Map.fromList (map typeToPair unions)
                      , Module.effects = canonicalEffects
                      }
                }



-- IMPORTS


filterImports
    :: Set.Set ModuleName.Raw
    -> ([Module.DefaultImport], [Module.UserImport])
    -> Result [ModuleName.Raw]
filterImports uses (defaults, imports) =
  let
    checkImport (A.A region (A.A _ name, _method)) =
      if Set.member name uses then
        return (Just name)

      else
        Result.warn region (Warning.UnusedImport name) Nothing
  in
    do  reducedImports <-
          Maybe.catMaybes <$> traverse checkImport imports

        return $
          Set.toList (Set.fromList (map fst defaults ++ reducedImports))



--------  CANONICALIZATION  --------


type CResult a =
  Canon.Result a



-- EXPORTS


resolveExports :: Exposing.Canonical -> Exposing.Raw -> CResult Exposing.Canonical
resolveExports internalApi@(Exposing.Canonical values aliases unions) rawExposing =
  case rawExposing of
    Exposing.Open ->
      Result.ok internalApi

    Exposing.Explicit entries ->
      let
        valueSet = Set.fromList values
        aliasSet = Set.fromList aliases
        unionMap = Map.fromList unions
      in
        case List.foldl' (resolveEntry valueSet aliasSet unionMap) (Exposing.nothing, []) entries of
          (exposing, []) ->
            do  checkForDups entries
                Result.ok exposing

          (_, errors) ->
            Result.throwMany errors


resolveEntry
  :: Set.Set Text
  -> Set.Set Text
  -> Map.Map Text [Text]
  -> (Exposing.Canonical, [A.Located CError.Error])
  -> A.Located Exposing.Entry
  -> (Exposing.Canonical, [A.Located CError.Error])
resolveEntry vSet aSet uSet (exposing@(Exposing.Canonical values aliases unions), errors) (A.A region entry) =
  case entry of
    Exposing.Lower name ->
      if Set.member name vSet then
        ( Exposing.Canonical (name:values) aliases unions
        , errors
        )

      else
        ( exposing
        , notFound region (Set.toList vSet) name : errors
        )

    Exposing.Upper name Nothing ->
      if Set.member name aSet then
        ( Exposing.Canonical
            (if Set.member name vSet then name:values else values)
            (name:aliases)
            unions
        , errors
        )

      else if Map.member name uSet then
        ( Exposing.Canonical values aliases ((name, []) : unions)
        , errors
        )

      else
        ( exposing
        , notFound region (Set.toList aSet ++ Map.keys uSet) name : errors
        )

    Exposing.Upper name (Just exposedCtors) ->
      case Map.lookup name uSet of
        Nothing ->
          ( exposing
          , notFound region (Map.keys uSet) name : errors
          )

        Just allCtors ->
          case exposedCtors of
            Exposing.Open ->
              ( Exposing.Canonical values aliases ((name, allCtors) : unions)
              , errors
              )

            Exposing.Explicit ctors ->
              let
                isNotKnown (A.A _ ctor) =
                  ctor `notElem` allCtors

                notKnown (A.A ctorRegion ctor) =
                  notFound ctorRegion allCtors ctor
              in
                case filter isNotKnown ctors of
                  [] ->
                    ( Exposing.Canonical values aliases ((name, map A.drop ctors) : unions)
                    , errors
                    )

                  unfoundCtors ->
                    ( exposing
                    , map notKnown unfoundCtors ++ errors
                    )


notFound :: Region.Region -> [Text] -> Text -> A.Located CError.Error
notFound region possibilities badName =
  A.A region $ CError.Export badName $
      Help.nearbyNames id badName possibilities


checkForDups :: [A.Located Exposing.Entry] -> CResult ()
checkForDups entries =
  let
    locations =
      A.listToDict Exposing.getName entries

    isUnique name (region :| duplicates) =
      case duplicates of
        [] ->
          Result.ok ()

        _ ->
          Result.throw region (CError.DuplicateExport name)
  in
    T.sequenceA_ (Map.mapWithKey isUnique locations)



-- ALL POSSIBLE EXPOSED VALUES


getInternalApi :: Effects.Raw -> D.Valid -> Exposing.Canonical
getInternalApi effects (D.Decls defs unions aliases _) =
  let
    values =
      Effects.toExposedValues effects
      ++ concatMap getAliasValues aliases
      ++ concatMap getDefValues defs
  in
    Exposing.Canonical values (map getAlias aliases) (map getUnion unions)


getDefValues :: A.Commented Src.ValidDef -> [Text]
getDefValues (A.A _ def) =
  P.boundVarList (Src.getPattern def)


getAliasValues :: A.Commented (D.Alias Type.Raw) -> [Text]
getAliasValues (A.A _ (D.Type name _ tipe)) =
  case tipe of
    A.A _ (Type.RRecord _ Nothing) ->
      [ name ]

    _ ->
      []


getAlias :: A.Commented (D.Alias Type.Raw) -> Text
getAlias (A.A _ (D.Type name _ _)) =
  name


getUnion :: A.Commented (D.Union Type.Raw) -> (Text, [Text])
getUnion (A.A _ (D.Type name _ ctors)) =
  (name, map fst ctors)



-- DECLARATIONS


canonicalizeDecls :: Env.Env -> D.Valid -> CResult D.Canonical
canonicalizeDecls env (D.Decls defs unions aliases infixes) =
  let
    annTraverse canEntry entries =
      traverse (\(A.A ann entry) -> A.A ann <$> canEntry entry) entries

    canonicalizeDef (Src.Def region pat expr typ) =
      C.Def region
        <$> canonicalizePattern env pat
        <*> canonicalizeExpr env expr
        <*> traverse (canonicalizeRegionType env) typ

    canonicalizeArgs (ctor, args) =
      (,) ctor <$> traverse (Canon.tipe env) args

    canonicalizeUnion (D.Type name tvars ctors) =
      D.Type name tvars <$> traverse canonicalizeArgs ctors

    canonicalizeAlias (D.Type name tvars alias) =
      D.Type name tvars <$> Canon.tipe env alias
  in
    D.Decls
      <$> annTraverse canonicalizeDef defs
      <*> annTraverse canonicalizeUnion unions
      <*> annTraverse canonicalizeAlias aliases
      <*> pure infixes


canonicalizeRegionType :: Env.Env -> Type.Raw -> CResult (A.Located Type.Canonical)
canonicalizeRegionType env typ@(A.A region _) =
  A.A region <$> Canon.tipe env typ



-- EXPRESSIONS


canonicalizeExpr :: Env.Env -> Src.ValidExpr -> CResult C.Expr
canonicalizeExpr env (A.A region validExpr) =
    let go = canonicalizeExpr env
    in
    A.A region <$>
    case validExpr of
      Src.Literal lit ->
          Result.ok (C.Literal lit)

      Src.Access record field ->
          C.Access <$> go record <*> Result.ok field

      Src.Update record fields ->
          C.Update <$> go record <*> traverse (canonicalizeFields env) fields

      Src.Record fields ->
          C.Record <$> traverse (canonicalizeFields env) fields

      Src.Binop ops last ->
          canonicalizeBinop region env ops last

      Src.Lambda arg body ->
          let
            env' =
              Env.addPattern arg env
          in
            C.Lambda
              <$> canonicalizePattern env' arg
              <*> canonicalizeExpr env' body

      Src.App func arg ->
          C.App <$> go func <*> go arg

      Src.If branches finally ->
          C.If
            <$> traverse go' branches
            <*> go finally
        where
          go' (condition, branch) =
              (,) <$> go condition <*> go branch

      Src.Let defs expr ->
          C.Let <$> traverse rename' defs <*> canonicalizeExpr env' expr
        where
          env' =
            foldr Env.addPattern env (map Src.getPattern defs)

          rename' (Src.Def defRegion p body mtipe) =
            C.Def defRegion
              <$> canonicalizePattern env' p
              <*> canonicalizeExpr env' body
              <*> traverse (canonicalizeRegionType env') mtipe

      Src.Var (Var.Raw x) ->
          C.Var <$> Canon.variable region env x

      Src.Ctor name exprs ->
          C.Ctor
            <$> Canon.variable region env name
            <*> traverse go exprs

      Src.List exprs ->
          C.List <$> traverse go exprs

      Src.Case expr cases ->
          C.Case <$> go expr <*> traverse branch cases
        where
          branch (ptrn, brnch) =
            (,)
              <$> canonicalizePattern env ptrn
              <*> canonicalizeExpr (Env.addPattern ptrn env) brnch

      Src.GLShader uid src tipe ->
          Result.ok (C.GLShader uid src tipe)


canonicalizeFields
  :: Env.Env
  -> (A.Located Text, Src.ValidExpr)
  -> CResult (Text, C.Expr)
canonicalizeFields env (A.A _ key, expr) =
  (\value -> (key, value)) <$> canonicalizeExpr env expr



-- PATTERNS


canonicalizePattern :: Env.Env -> P.Raw -> CResult P.Canonical
canonicalizePattern env (A.A region ptrn) =
  A.A region <$>
    case ptrn of
      P.Var x ->
          Result.ok (P.Var x)

      P.Literal lit ->
          Result.ok (P.Literal lit)

      P.Record fields ->
          Result.ok (P.Record fields)

      P.Anything ->
          Result.ok P.Anything

      P.Alias x p ->
          P.Alias x <$> canonicalizePattern env p

      P.Ctor (Var.Raw name) patterns ->
          P.Ctor
            <$> Canon.pvar region env name (length patterns)
            <*> traverse (canonicalizePattern env) patterns



-- BINOPS


canonicalizeBinop
    :: Region.Region
    -> Env.Env
    -> [(Src.ValidExpr, A.Located Text)]
    -> Src.ValidExpr
    -> CResult C.Expr'
canonicalizeBinop region env srcOps srcLast =
  let
    opsResult =
      traverse (binopHelp env) srcOps

    lastResult =
      canonicalizeExpr env srcLast
  in
    do  (expr, ops) <- (,) <$> opsResult <*> lastResult
        A.drop <$> Binops.flatten region expr ops


binopHelp :: Env.Env -> (Src.ValidExpr, A.Located Text) -> CResult (C.Expr, Binops.Op)
binopHelp env (expr, op) =
  (,)
    <$> canonicalizeExpr env expr
    <*> Binops.canonicalize env op
