module Canonicalize (module') where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Traversable as T
import qualified Data.Foldable as T

import AST.Expression.General (Expr'(..), dummyLet)
import Elm.Utils ((|>))

import qualified AST.Declaration as D
import qualified AST.Expression.Valid as Valid
import qualified AST.Expression.Canonical as Canonical
import qualified AST.Module as Module
import qualified AST.Module.Name as ModuleName
import qualified AST.Pattern as P
import qualified AST.Type as Type
import qualified AST.Variable as Var
import qualified Docs.Centralize as Docs
import qualified Reporting.Annotation as A
import qualified Reporting.Error as Error
import qualified Reporting.Error.Canonicalize as CError
import qualified Reporting.Error.Helpers as ErrorHelp
import qualified Reporting.Region as Region
import qualified Reporting.Result as R
import qualified Reporting.Warning as Warning
import qualified Canonicalize.Declaration as Decls
import qualified Canonicalize.Environment as Env
import qualified Canonicalize.Result as Result
import qualified Canonicalize.Setup as Setup
import qualified Canonicalize.Sort as Sort
import qualified Canonicalize.Type as Canonicalize
import qualified Canonicalize.Variable as Canonicalize



-- MODULES


module'
    :: [ModuleName.Canonical]
    -> Module.Interfaces
    -> Module.Valid
    -> R.Result Warning.Warning Error.Error Module.Canonical
module' canonicalImports interfaces modul =
  let
    (Module.Valid docs exports imports decls _) =  -- TODO do something with `effects`
      Module.info modul

    importDict =
      canonicalImports
        |> map (\cName -> (ModuleName._module cName, cName))
        |> Map.fromList

    canonicalDeclsResult =
      Setup.environment importDict interfaces modul
        `Result.andThen` \env -> (,) env <$> T.traverse (declaration env) decls

    (Result.Result uses rawResults) =
      (,)
        <$> canonicalDeclsResult
        <*> resolveExports (concatMap declToValues decls) exports
  in
    case rawResults of
      Result.Err msgs ->
        R.throwMany (map (A.map Error.Canonicalize) msgs)

      Result.Ok ((env, canonicalDecls), canonicalExports) ->
        let
          nakedDecls =
            map A.drop canonicalDecls

          centralizedDocs =
            A.map (fmap (Docs.centralize canonicalDecls)) docs

          program =
            Sort.definitions (dummyLet (Decls.toExpr (Module.name modul) canonicalDecls))

          fixities =
            [ (assoc,level,op) | D.Fixity assoc level op <- nakedDecls ]

          aliases =
            Map.fromList [ (name,(tvs,alias)) | D.Alias name tvs alias <- nakedDecls ]

          unions =
            Map.fromList [ (name,(vars,ctors)) | D.Union name vars ctors <- nakedDecls ]
        in
          R.addDealiaser (Env.toDealiaser env) $
            do  canonicalImports <- filterImports uses imports
                return $ modul {
                  Module.info =
                    Module.Info
                      { Module.docs = centralizedDocs
                      , Module.exports = canonicalExports
                      , Module.imports = canonicalImports
                      , Module.program = program
                      , Module.types = Map.empty
                      , Module.fixities = fixities
                      , Module.aliases = aliases
                      , Module.unions = unions
                      }
                  }



-- IMPORTS


filterImports
    :: Set.Set ModuleName.Raw
    -> ([Module.DefaultImport], [Module.UserImport])
    -> R.Result Warning.Warning e [ModuleName.Raw]
filterImports uses (defaults, imports) =
  let
    checkImport (A.A region (name, _method)) =
      if Set.member name uses then
        return (Just name)

      else
        do  R.warn region (Warning.UnusedImport name)
            return Nothing
  in
    do  reducedImports <-
          Maybe.catMaybes <$> T.traverse checkImport imports

        return $
          Set.toList (Set.fromList (map fst defaults ++ reducedImports))



-- EXPORTS


resolveExports
    :: [Var.Value]
    -> Var.Listing (A.Located Var.Value)
    -> Result.ResultErr [Var.Value]
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
        <$> T.traverse (getValueExport allValues (Set.fromList allValues)) values
        <*> (concat <$> T.traverse (getAliasExport allValues allAliases adtTypes) aliases)
        <*> T.traverse (getAdtExport allAdts adtTypes) adts
        <*> allUnique partialList


getValueExport
    :: [String]
    -> Set.Set String
    -> A.Located String
    -> Result.ResultErr Var.Value
getValueExport allValues allValuesSet (A.A region name) =
  if Set.member name allValuesSet then
    Result.ok (Var.Value name)
  else
    manyNotFound region [name] allValues


getAliasExport
    :: [String]
    -> [String]
    -> [String]
    -> A.Located String
    -> Result.ResultErr [Var.Value]
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
    -> Result.ResultErr Var.Value
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


manyNotFound :: Region.Region -> [String] -> [String] -> Result.ResultErr a
manyNotFound region nameList possibilities =
    Result.errors (map (notFound region possibilities) nameList)


notFound :: Region.Region -> [String] -> String -> A.Located CError.Error
notFound region possibilities name =
    A.A region $ CError.Export name $
        ErrorHelp.nearbyNames id name possibilities


allUnique :: [A.Located Var.Value] -> Result.ResultErr ()
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
              Result.err (A.A region (CError.DuplicateExport (valueToString value)))

          _ ->
              Result.ok ()
  in
    T.traverse_ id (Map.mapWithKey isUnique locations)



-- CONVERSIONS


declToValues :: D.Valid -> [Var.Value]
declToValues (A.A _ decl) =
  case decl of
    D.Def def ->
      map Var.Value (P.boundVarList (Valid.getPattern def))

    D.Union name _tvs ctors ->
      [ Var.Union name (Var.Listing (map fst ctors) False) ]

    D.Alias name _ tipe ->
      case tipe of
        A.A _ (Type.RRecord _ Nothing) ->
          [ Var.Alias name, Var.Value name ]

        _ ->
          [ Var.Alias name ]

    _ ->
      []



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


declaration :: Env.Environment -> D.Valid -> Result.ResultErr D.Canonical
declaration env (A.A ann decl) =
  A.A ann <$>
    case decl of
      D.Def (Valid.Definition pat expr typ) ->
          D.Def <$> (
              Canonical.Definition Canonical.dummyFacts
                <$> pattern env pat
                <*> expression env expr
                <*> T.traverse (regionType env) typ
          )

      D.Union name tvars ctors ->
          D.Union name tvars <$> T.traverse canonicalizeArgs ctors
        where
          canonicalizeArgs (ctor, args) =
              (,) ctor <$> T.traverse (Canonicalize.tipe env) args

      D.Alias name tvars expanded ->
          D.Alias name tvars
            <$> Canonicalize.tipe env expanded

      D.Fixity assoc prec op ->
          Result.ok (D.Fixity assoc prec op)


regionType
    :: Env.Environment
    -> Type.Raw
    -> Result.ResultErr (A.Located Type.Canonical)
regionType env typ@(A.A region _) =
  A.A region <$> Canonicalize.tipe env typ


expression
    :: Env.Environment
    -> Valid.Expr
    -> Result.ResultErr Canonical.Expr
expression env (A.A region validExpr) =
    let go = expression env
    in
    A.A region <$>
    case validExpr of
      Literal lit ->
          Result.ok (Literal lit)

      Range lowExpr highExpr ->
          Range <$> go lowExpr <*> go highExpr

      Access record field ->
          Access <$> go record <*> Result.ok field

      Update record fields ->
          Update
            <$> go record
            <*> T.traverse (\(field,expr) -> (,) field <$> go expr) fields

      Record fields ->
          Record
            <$> T.traverse (\(field,expr) -> (,) field <$> go expr) fields

      Binop (Var.Raw op) leftExpr rightExpr ->
          Binop
            <$> Canonicalize.variable region env op
            <*> go leftExpr
            <*> go rightExpr

      Lambda arg body ->
          let
            env' =
              Env.addPattern arg env
          in
            Lambda <$> pattern env' arg <*> expression env' body

      App func arg ->
          App <$> go func <*> go arg

      If branches finally ->
          If
            <$> T.traverse go' branches
            <*> go finally
        where
          go' (condition, branch) =
              (,) <$> go condition <*> go branch

      Let defs body ->
          Let <$> T.traverse rename' defs <*> expression env' body
        where
          env' =
            foldr Env.addPattern env $ map (\(Valid.Definition p _ _) -> p) defs

          rename' (Valid.Definition p body mtipe) =
            Canonical.Definition Canonical.dummyFacts
              <$> pattern env' p
              <*> expression env' body
              <*> T.traverse (regionType env') mtipe

      Var (Var.Raw x) ->
          Var <$> Canonicalize.variable region env x

      Data name exprs ->
          Data name <$> T.traverse go exprs

      ExplicitList exprs ->
          ExplicitList <$> T.traverse go exprs

      Case expr cases ->
          Case <$> go expr <*> T.traverse branch cases
        where
          branch (ptrn, brnch) =
            (,)
              <$> pattern env ptrn
              <*> expression (Env.addPattern ptrn env) brnch

      GLShader uid src tipe ->
          Result.ok (GLShader uid src tipe)


pattern :: Env.Environment -> P.Raw -> Result.ResultErr P.Canonical
pattern env (A.A region ptrn) =
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
          P.Alias x <$> pattern env p

      P.Data (Var.Raw name) patterns ->
          P.Data
            <$> Canonicalize.pvar region env name (length patterns)
            <*> T.traverse (pattern env) patterns
