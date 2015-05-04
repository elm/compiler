module Transform.Canonicalize (module') where

import Control.Applicative ((<$>),(<*>))
import Control.Monad.Except (throwError)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Traversable as T

import AST.Expression.General (Expr'(..), dummyLet)
import AST.Module (CanonicalBody(..))

import qualified AST.Declaration as D
import qualified AST.Expression.General as E
import qualified AST.Expression.Valid as Valid
import qualified AST.Expression.Canonical as Canonical
import qualified AST.Module as Module
import qualified AST.Pattern as P
import qualified AST.Type as Type
import qualified AST.Variable as Var
import qualified Reporting.Annotation as A
import qualified Reporting.Error as Error
import qualified Reporting.Error.Canonicalize as CError
import qualified Reporting.Task as Task
import qualified Reporting.Warning as Warning
import qualified Transform.Canonicalize.Environment as Env
import qualified Transform.Canonicalize.Port as Port
import qualified Transform.Canonicalize.Result as Result
import qualified Transform.Canonicalize.Setup as Setup
import qualified Transform.Canonicalize.Type as Canonicalize
import qualified Transform.Canonicalize.Variable as Canonicalize
import qualified Transform.SortDefinitions as Transform
import qualified Transform.Declaration as Transform


-- MODULES

module'
    :: Module.Interfaces
    -> Module.ValidModule
    -> Task.Task Warning.Warning Error.Error Module.CanonicalModule
module' interfaces modul@(Module.Module _ _ _ (_, imports) _) =
  let (Result.Result uses rawResults) =
          moduleHelp interfaces modul

  in
      do  Task.addWarnings (Maybe.mapMaybe (warnUnusedImport uses) imports)
          case rawResults of
            Result.Ok canonicalModule ->
                return canonicalModule

            Result.Err msgs ->
                throwError (map (A.map Error.Canonicalize) msgs)


moduleHelp
    :: Module.Interfaces
    -> Module.ValidModule
    -> Result.ResultErr Module.CanonicalModule
moduleHelp interfaces modul@(Module.Module _ _ exports (defaults, imports) decls) =
    canonicalModule
      <$> canonicalDeclsResult
      <*> resolveExports locals exports
  where
    canonicalModule canonicalDecls canonicalExports =
        modul
          { Module.exports = canonicalExports
          , Module.imports = map fst defaults ++ map (fst . A.drop) imports
          , Module.body = body canonicalDecls
          }

    locals :: [Var.Value]
    locals =
        concatMap declToValue decls

    canonicalDeclsResult =
        Setup.environment interfaces modul
          `Result.andThen` \env -> T.traverse (declaration env) decls

    body :: [D.CanonicalDecl] -> Module.CanonicalBody
    body decls =
        let nakedDecls = map A.drop decls
        in
        Module.CanonicalBody
          { program =
              let expr = Transform.toExpr (Module.names modul) decls
              in
                  Transform.sortDefs (dummyLet expr)

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


-- IMPORTS / EXPORTS

warnUnusedImport
    :: Set.Set Module.Name
    -> Module.UserImport
    -> Maybe (A.Located Warning.Warning)
warnUnusedImport uses (A.A region (name, _method)) =
  if Set.member name uses
    then Nothing
    else Just (A.A region (Warning.UnusedImport name))


resolveExports
    :: [Var.Value]
    -> Var.Listing Var.Value
    -> Result.ResultErr [Var.Value]
resolveExports fullList (Var.Listing partialList open) =
  if open
    then Result.ok fullList
    else
      (\xs ys zs -> xs ++ ys ++ zs)
        <$> getValueExports
        <*> (concat <$> T.traverse getAliasExport aliases)
        <*> T.traverse getAdtExport adts
  where
    (allValues, allAliases, allAdts) =
        splitValues fullList

    (values, aliases, adts) =
        splitValues partialList

    getValueExports =
      case Set.toList (Set.difference values' allValues') of
        [] -> Result.ok (map Var.Value values)
        xs -> manyNotFound xs allValues
      where
        allValues' = Set.fromList allValues
        values' = Set.fromList values

    getAliasExport alias
        | alias `elem` allAliases =
            let recordConstructor =
                    if alias `elem` allValues then [Var.Value alias] else []
            in
                Result.ok (Var.Alias alias : recordConstructor)

        | otherwise =
            case List.find (\(name, _ctors) -> name == alias) allAdts of
              Nothing -> manyNotFound [alias] allAliases
              Just (name, _ctor) ->
                  Result.ok [Var.Union name (Var.Listing [] False)]

    getAdtExport (name, Var.Listing ctors open) =
      case lookup name allAdts of
        Nothing -> manyNotFound [name] (map fst allAdts)
        Just (Var.Listing allCtors _)
          | open ->
              Result.ok (Var.Union name (Var.Listing allCtors False))
          | otherwise ->
              case filter (`notElem` allCtors) ctors of
                [] -> Result.ok (Var.Union name (Var.Listing ctors False))
                unfoundCtors -> manyNotFound unfoundCtors allCtors

    manyNotFound nameList possibilities =
        Result.errors (map (notFound possibilities) nameList)

    notFound possibilities name =
        A.A undefined $ CError.Export name $
            CError.nearbyNames id name possibilities


splitValues
    :: [Var.Value]
    -> ([String], [String], [(String, Var.Listing String)])
splitValues mixedValues =
  case mixedValues of
    [] -> ([], [], [])
    x:xs ->
      let (values, aliases, adts) = splitValues xs in
      case x of
        Var.Value name ->
            (name : values, aliases, adts)

        Var.Alias name ->
            (values, name : aliases, adts)

        Var.Union name listing ->
            (values, aliases, (name, listing) : adts)


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
declaration env (A.A region decl) =
    let canonicalize kind _context _pattern env v =
            kind env v
    in
    A.A region <$>
    case decl of
      D.Definition (Valid.Definition p e t) ->
          D.Definition <$> (
              Canonical.Definition
                <$> canonicalize pattern "definition" p env p
                <*> expression env e
                <*> T.traverse (canonicalize Canonicalize.tipe "definition" p env) t
          )

      D.Datatype name tvars ctors ->
          D.Datatype name tvars <$> T.traverse canonicalize' ctors
        where
          canonicalize' (ctor,args) =
              (,) ctor <$> T.traverse (canonicalize Canonicalize.tipe "datatype" name env) args

      D.TypeAlias name tvars expanded ->
          D.TypeAlias name tvars
            <$> canonicalize Canonicalize.tipe "type alias" name env expanded

      D.Port validPort ->
          Result.addModule ["Native","Port"] $
          Result.addModule ["Native","Json"] $
              case validPort of
                D.In name tipe ->
                    canonicalize Canonicalize.tipe "port" name env tipe
                      `Result.andThen` \canonicalType ->
                          D.Port <$> Port.check name Nothing canonicalType

                D.Out name expr tipe ->
                    let exprTypeResult =
                          (,)
                            <$> expression env expr
                            <*> canonicalize Canonicalize.tipe "port" name env tipe
                    in
                        exprTypeResult
                          `Result.andThen` \(expr', tipe') ->
                              D.Port <$> Port.check name (Just expr') tipe'

      D.Fixity assoc prec op ->
          Result.ok (D.Fixity assoc prec op)


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

      Remove record field ->
          flip Remove field <$> go record

      Insert record field expr ->
          flip Insert field <$> go record <*> go expr

      Modify record fields ->
          Modify
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

      MultiIf branches ->
          MultiIf <$> T.traverse go' branches
        where
          go' (condition, branch) =
              (,) <$> go condition <*> go branch

      Let defs body ->
          Let <$> T.traverse rename' defs <*> expression env' body
        where
          env' =
              foldr Env.addPattern env $ map (\(Valid.Definition p _ _) -> p) defs

          rename' (Valid.Definition p body mtipe) =
              Canonical.Definition
                  <$> pattern env' p
                  <*> expression env' body
                  <*> T.traverse (Canonicalize.tipe env') mtipe

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
