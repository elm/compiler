{-# OPTIONS_GHC -Wall #-}
module Canonicalize (module') where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Foldable as T

import AST.Expression.General (Expr'(..))
import Elm.Utils ((|>))

import qualified AST.Declaration as D
import qualified AST.Expression.Valid as Valid
import qualified AST.Expression.Canonical as Canonical
import qualified AST.Module as Module
import qualified AST.Module.Name as ModuleName
import qualified AST.Pattern as P
import qualified AST.Type as Type
import qualified AST.Variable as Var
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
import qualified Reporting.Error.Helpers as ErrorHelp
import qualified Reporting.Region as Region
import qualified Reporting.Render.Type as RenderType
import qualified Reporting.Result as Result
import qualified Reporting.Warning as Warning



type Result a =
  Result.Result (Result.One RenderType.Localizer) Warning.Warning Error.Error a


-- MODULES


module'
    :: [ModuleName.Canonical]
    -> Module.Interfaces
    -> Module.Valid
    -> Result Module.Canonical
module' allCanonicalImports interfaces modul =
  let
    (Module.Valid docs exports imports decls effects) =
      Module.info modul

    importDict =
      allCanonicalImports
        |> map (\cName -> (ModuleName._module cName, cName))
        |> Map.fromList

    (Result.Result uses warnings rawResults) =
      do  env <- Setup.environment importDict interfaces modul

          (,,,) env
            <$> resolveExports (Effects.toValues effects ++ declsToValues decls) exports
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
              Result.accumulate (Result.One (Env.toDealiaser env)) $
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
    checkImport (A.A region (name, _method)) =
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


resolveExports :: [Var.Value] -> Var.Listing (A.Located Var.Value) -> CResult [Var.Value]
resolveExports fullList (Var.Listing partialList open) =
  if open then
    Result.ok fullList

  else
    let
      (allValues, allAliases, allAdts) =
          maybeUnzip3 (map splitValue fullList)

      (values, aliases, adts) =
          maybeUnzip3 (map splitLocatedValue partialList)

      adtTypes =
          map fst allAdts
    in
      (\xs ys zs _ -> xs ++ ys ++ zs)
        <$> traverse (getValueExport allValues (Set.fromList allValues)) values
        <*> (concat <$> traverse (getAliasExport allValues allAliases adtTypes) aliases)
        <*> traverse (getAdtExport allAdts adtTypes) adts
        <*> allUnique partialList


getValueExport :: [String] -> Set.Set String -> A.Located String -> CResult Var.Value
getValueExport allValues allValuesSet (A.A region name) =
  if Set.member name allValuesSet then
    Result.ok (Var.Value name)
  else
    manyNotFound region [name] allValues


getAliasExport :: [String] -> [String] -> [String] -> A.Located String -> CResult [Var.Value]
getAliasExport allValues allAliases adtTypes (A.A region alias) =
  if alias `elem` allAliases then

      Result.ok $ (:) (Var.Alias alias) $
          if alias `elem` allValues then [Var.Value alias] else []

  else if List.elem alias adtTypes then

      Result.ok [Var.Union alias (Var.Listing [] False)]

  else

      manyNotFound region [alias] (allAliases ++ adtTypes)


getAdtExport
    :: [(String, Var.Listing String)]
    -> [String]
    -> A.Located (String, Var.Listing String)
    -> CResult Var.Value
getAdtExport allAdts adtTypes (A.A region (name, Var.Listing ctors open)) =
  case List.lookup name allAdts of
    Nothing ->
        manyNotFound region [name] adtTypes

    Just (Var.Listing allCtors _) ->
        if open then
            Result.ok (Var.Union name (Var.Listing allCtors False))
        else
          case filter (`notElem` allCtors) ctors of
            [] ->
                Result.ok (Var.Union name (Var.Listing ctors False))
            unfoundCtors ->
                manyNotFound region unfoundCtors allCtors


manyNotFound :: Region.Region -> [String] -> [String] -> CResult a
manyNotFound region nameList possibilities =
    Result.throwMany (map (notFound region possibilities) nameList)


notFound :: Region.Region -> [String] -> String -> A.Located CError.Error
notFound region possibilities name =
    A.A region $ CError.Export name $
        ErrorHelp.nearbyNames id name possibilities


allUnique :: [A.Located Var.Value] -> CResult ()
allUnique statedExports =
  let
    valueToString value =
        case value of
          Var.Value name -> name
          Var.Alias name -> name
          Var.Union name _ -> name

    locations =
        Map.fromListWith (++) (map (\(A.A region value) -> (value, [region])) statedExports)

    isUnique value allRegions =
        case allRegions of
          region : _ : _ ->
              Result.throw region (CError.DuplicateExport (valueToString value))

          _ ->
              Result.ok ()
  in
    T.sequenceA_ (Map.mapWithKey isUnique locations)



-- CONVERSIONS


declsToValues :: D.Valid -> [Var.Value]
declsToValues (D.Decls defs unions aliases _) =
  let
    fromDef (A.A _ def) =
      map Var.Value (P.boundVarList (Valid.getPattern def))

    fromUnion (A.A _ (D.Type name _ ctors)) =
      Var.Union name (Var.Listing (map fst ctors) False)

    fromAlias (A.A _ (D.Type name _ tipe)) =
      case tipe of
        A.A _ (Type.RRecord _ Nothing) ->
          [ Var.Alias name, Var.Value name ]

        _ ->
          [ Var.Alias name ]
  in
    concat
      [ map fromUnion unions
      , concatMap fromAlias aliases
      , concatMap fromDef defs
      ]



-- GROUPING VALUES


maybeUnzip3 :: [(Maybe a, Maybe b, Maybe c)] -> ([a],[b],[c])
maybeUnzip3 tuples =
  let (as, bs, cs) = unzip3 tuples
  in
    (Maybe.catMaybes as, Maybe.catMaybes bs, Maybe.catMaybes cs)


splitValue
    :: Var.Value
    -> ( Maybe String, Maybe String, Maybe (String, Var.Listing String) )
splitValue value =
  case value of
    Var.Value name ->
        (Just name, Nothing, Nothing)

    Var.Alias name ->
        (Nothing, Just name, Nothing)

    Var.Union name listing ->
        (Nothing, Nothing, Just (name, listing))


splitLocatedValue
    :: A.Located Var.Value
    ->
      ( Maybe (A.Located String)
      , Maybe (A.Located String)
      , Maybe (A.Located (String, Var.Listing String))
      )
splitLocatedValue (A.A region value) =
  case value of
    Var.Value name ->
        (Just (A.A region name), Nothing, Nothing)

    Var.Alias name ->
        (Nothing, Just (A.A region name), Nothing)

    Var.Union name listing ->
        (Nothing, Nothing, Just (A.A region (name, listing)))



-- DECLARATIONS


canonicalizeDecls :: Env.Environment -> D.Valid -> CResult D.Canonical
canonicalizeDecls env (D.Decls defs unions aliases infixes) =
  let
    annTraverse canEntry entries =
      traverse (\(A.A ann entry) -> A.A ann <$> canEntry entry) entries

    canonicalizeDef (Valid.Def region pat expr typ) =
      Canonical.Def region
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


canonicalizeRegionType
    :: Env.Environment
    -> Type.Raw
    -> CResult (A.Located Type.Canonical)
canonicalizeRegionType env typ@(A.A region _) =
  A.A region <$> Canon.tipe env typ


canonicalizeExpr
    :: Env.Environment
    -> Valid.Expr
    -> CResult Canonical.Expr
canonicalizeExpr env (A.A region validExpr) =
    let go = canonicalizeExpr env
    in
    A.A region <$>
    case validExpr of
      Literal lit ->
          Result.ok (Literal lit)

      Access record field ->
          Access <$> go record <*> Result.ok field

      Update record fields ->
          Update
            <$> go record
            <*> traverse (\(field,expr) -> (,) field <$> go expr) fields

      Record fields ->
          Record
            <$> traverse (\(field,expr) -> (,) field <$> go expr) fields

      Binop (Var.Raw op) leftExpr rightExpr ->
          Binop
            <$> Canon.variable region env op
            <*> go leftExpr
            <*> go rightExpr

      Lambda arg body ->
          let
            env' =
              Env.addPattern arg env
          in
            Lambda <$> canonicalizePattern env' arg <*> canonicalizeExpr env' body

      App func arg ->
          App <$> go func <*> go arg

      If branches finally ->
          If
            <$> traverse go' branches
            <*> go finally
        where
          go' (condition, branch) =
              (,) <$> go condition <*> go branch

      Let defs expr ->
          Let <$> traverse rename' defs <*> canonicalizeExpr env' expr
        where
          env' =
            foldr Env.addPattern env (map Valid.getPattern defs)

          rename' (Valid.Def defRegion p body mtipe) =
            Canonical.Def defRegion
              <$> canonicalizePattern env' p
              <*> canonicalizeExpr env' body
              <*> traverse (canonicalizeRegionType env') mtipe

      Var (Var.Raw x) ->
          Var <$> Canon.variable region env x

      Data name exprs ->
          Data name <$> traverse go exprs

      ExplicitList exprs ->
          ExplicitList <$> traverse go exprs

      Case expr cases ->
          Case <$> go expr <*> traverse branch cases
        where
          branch (ptrn, brnch) =
            (,)
              <$> canonicalizePattern env ptrn
              <*> canonicalizeExpr (Env.addPattern ptrn env) brnch

      Cmd moduleName ->
          Result.ok (Cmd moduleName)

      Sub moduleName ->
          Result.ok (Sub moduleName)

      OutgoingPort name tipe ->
          OutgoingPort name <$> Canon.tipe env tipe

      IncomingPort name tipe ->
          IncomingPort name <$> Canon.tipe env tipe

      Program _ _ ->
          error "DANGER - Program AST nodes should not be in canonicalization."

      SaveEnv moduleName effects ->
          Result.ok (SaveEnv moduleName effects)

      GLShader uid src tipe ->
          Result.ok (GLShader uid src tipe)


canonicalizePattern :: Env.Environment -> P.Raw -> CResult P.Canonical
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

      P.Data (Var.Raw name) patterns ->
          P.Data
            <$> Canon.pvar region env name (length patterns)
            <*> traverse (canonicalizePattern env) patterns
