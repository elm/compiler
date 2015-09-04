module Canonicalize (module') where

import Control.Applicative ((<$>),(<*>))
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Traversable as T
import qualified Data.Foldable as T

import AST.Expression.General (Expr'(..), dummyLet)
import AST.Module (Body(..))
import Elm.Utils ((|>))

import qualified AST.Declaration as D
import qualified AST.Expression.General as E
import qualified AST.Expression.Valid as Valid
import qualified AST.Expression.Canonical as Canonical
import qualified AST.Module as Module
import qualified AST.Module.Name as ModuleName
import qualified AST.Pattern as P
import qualified AST.Type as Type
import qualified AST.Variable as Var
import qualified Docs.AST as Docs
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
import qualified Canonicalize.Port as Port
import qualified Canonicalize.Result as Result
import qualified Canonicalize.Setup as Setup
import qualified Canonicalize.Sort as Sort
import qualified Canonicalize.Type as Canonicalize
import qualified Canonicalize.Variable as Canonicalize


-- MODULES

module'
    :: [ModuleName.Canonical]
    -> Module.Interfaces
    -> Module.ValidModule
    -> R.Result Warning.Warning Error.Error Module.CanonicalModule
module' canonicalImports interfaces modul =
  let
    importDict =
      canonicalImports
        |> map (\cName -> (ModuleName._module cName, cName))
        |> Map.fromList

    (Result.Result uses rawResults) =
      moduleHelp importDict interfaces modul
  in
      case rawResults of
        Result.Ok (env, almostCanonicalModule) ->
            R.addDealiaser (Env.toDealiaser env) $
              filterImports uses almostCanonicalModule

        Result.Err msgs ->
            R.throwMany (map (A.map Error.Canonicalize) msgs)


type AlmostCanonicalModule =
    Module.Module
      Docs.Centralized
      ([Module.DefaultImport], [Module.UserImport])
      [Var.Value]
      (Module.Body Canonical.Expr)


moduleHelp
    :: Map.Map ModuleName.Raw ModuleName.Canonical
    -> Module.Interfaces
    -> Module.ValidModule
    -> Result.ResultErr (Env.Environment, AlmostCanonicalModule)
moduleHelp importDict interfaces modul@(Module.Module _ _ comment exports _ decls) =
    canonicalModule
      <$> canonicalDeclsResult
      <*> resolveExports locals exports
  where
    canonicalModule (env, canonicalDecls) canonicalExports =
        (,) env $
        modul
          { Module.docs = A.map (fmap (Docs.centralize canonicalDecls)) comment
          , Module.exports = canonicalExports
          , Module.body = body canonicalDecls
          }

    locals :: [Var.Value]
    locals =
        concatMap declToValue decls

    canonicalDeclsResult =
        Setup.environment importDict interfaces modul
          `Result.andThen` \env -> (,) env <$> T.traverse (declaration env) decls

    body :: [D.CanonicalDecl] -> Module.Body Canonical.Expr
    body decls =
        let nakedDecls = map A.drop decls
        in
        Module.Body
          { program =
              let expr = Decls.toExpr (Module.name modul) decls
              in
                  Sort.definitions (dummyLet expr)

          , types =
              Map.empty

          , datatypes =
              Map.fromList [ (name,(vars,ctors)) | D.Datatype name vars ctors <- nakedDecls ]

          , fixities =
              [ (assoc,level,op) | D.Fixity assoc level op <- nakedDecls ]

          , aliases =
              Map.fromList [ (name,(tvs,alias)) | D.TypeAlias name tvs alias <- nakedDecls ]

          , ports =
              [ E.portName impl | D.Port (D.CanonicalPort impl) <- nakedDecls ]
          }


-- IMPORTS

filterImports
    :: Set.Set ModuleName.Raw
    -> AlmostCanonicalModule
    -> R.Result Warning.Warning e Module.CanonicalModule
filterImports uses modul@(Module.Module _ _ _ _ (defaults, imports) _) =
  do  reducedImports <-
          Maybe.catMaybes <$> T.traverse checkImport imports

      return $ modul
        { Module.imports =
              Set.toList (Set.fromList (map fst defaults ++ reducedImports))
        }
  where
    checkImport (A.A region (name, _method)) =
      if Set.member name uses then
          return (Just name)
      else
          do  R.warn region (Warning.UnusedImport name)
              return Nothing


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

declToValue :: D.ValidDecl -> [Var.Value]
declToValue (A.A _ decl) =
    case decl of
      D.Definition (Valid.Definition pattern _ _) ->
          map Var.Value (P.boundVarList pattern)

      D.Datatype name _tvs ctors ->
          [ Var.Union name (Var.Listing (map fst ctors) False) ]

      D.TypeAlias name _ tipe ->
          case tipe of
            A.A _ (Type.RRecord _ _) ->
                [ Var.Alias name, Var.Value name ]
            _ -> [ Var.Alias name ]

      _ -> []


declaration
    :: Env.Environment
    -> D.ValidDecl
    -> Result.ResultErr D.CanonicalDecl
declaration env (A.A ann@(region,_) decl) =
    A.A ann <$>
    case decl of
      D.Definition (Valid.Definition pat expr typ) ->
          D.Definition <$> (
              Canonical.Definition Canonical.dummyFacts
                <$> pattern env pat
                <*> expression env expr
                <*> T.traverse (regionType env) typ
          )

      D.Datatype name tvars ctors ->
          D.Datatype name tvars <$> T.traverse canonicalize' ctors
        where
          canonicalize' (ctor,args) =
              (,) ctor <$> T.traverse (Canonicalize.tipe env) args

      D.TypeAlias name tvars expanded ->
          D.TypeAlias name tvars
            <$> Canonicalize.tipe env expanded

      D.Port validPort ->
          Result.addModule ["Native","Port"] $
          Result.addModule ["Native","Json"] $
              case validPort of
                D.In name tipe ->
                    Canonicalize.tipe env tipe
                      `Result.andThen` \canonicalType ->
                          D.Port <$> Port.check region name Nothing canonicalType

                D.Out name expr tipe ->
                    let exprTypeResult =
                          (,)
                            <$> expression env expr
                            <*> Canonicalize.tipe env tipe
                    in
                        exprTypeResult
                          `Result.andThen` \(expr', tipe') ->
                              D.Port <$> Port.check region name (Just expr') tipe'

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
          let env' = Env.addPattern arg env
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
              (,) <$> pattern env ptrn
                  <*> expression (Env.addPattern ptrn env) brnch

      Port impl ->
          let portType pt =
                case pt of
                  Type.Normal t ->
                      Type.Normal
                          <$> Canonicalize.tipe env t

                  Type.Signal root arg ->
                      Type.Signal
                          <$> Canonicalize.tipe env root
                          <*> Canonicalize.tipe env arg
          in
              Port <$>
                  case impl of
                    E.In name tipe ->
                        E.In name <$> portType tipe

                    E.Out name expr tipe ->
                        E.Out name <$> go expr <*> portType tipe

                    E.Task name expr tipe ->
                        E.Task name <$> go expr <*> portType tipe

      GLShader uid src tipe ->
          Result.ok (GLShader uid src tipe)


pattern
    :: Env.Environment
    -> P.RawPattern
    -> Result.ResultErr P.CanonicalPattern
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
