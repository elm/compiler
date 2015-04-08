module Transform.Canonicalize (module') where

import Control.Applicative ((<$>),(<*>))
import Control.Monad.Error (runErrorT, throwError)
import Control.Monad.State (runState)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Traversable as T

import AST.Expression.General (Expr'(..), dummyLet)
import qualified AST.Expression.General as E
import qualified AST.Expression.Valid as Valid
import qualified AST.Expression.Canonical as Canonical

import AST.Module (CanonicalBody(..))
import qualified AST.Module as Module
import qualified AST.Type as Type
import qualified AST.Variable as Var
import qualified AST.Annotation as A
import qualified AST.Declaration as D
import AST.PrettyPrint (pretty, commaSep)
import qualified AST.Pattern as P
import Text.PrettyPrint as P

import qualified Transform.Canonicalize.Environment as Env
import qualified Transform.Canonicalize.Setup as Setup
import qualified Transform.Canonicalize.Type as Canonicalize
import qualified Transform.Canonicalize.Variable as Canonicalize
import qualified Transform.Canonicalize.Port as Port
import qualified Transform.SortDefinitions as Transform
import qualified Transform.Declaration as Transform


module'
    :: Module.Interfaces
    -> Module.ValidModule
    -> Either [Doc] Module.CanonicalModule
module' interfaces modul =
    filterImports <$> modul'
  where
    (modul', usedModules) =
        runState (runErrorT (moduleHelp interfaces modul)) Set.empty

    filterImports m =
        let used (name,_) = Set.member name usedModules
        in
            m { Module.imports = filter used (Module.imports m) }


moduleHelp
    :: Module.Interfaces
    -> Module.ValidModule
    -> Env.Canonicalizer [Doc] Module.CanonicalModule
moduleHelp interfaces modul@(Module.Module _ _ exports _ decls) =
  do  env <- Setup.environment interfaces modul
      canonicalDecls <- mapM (declaration env) decls
      exports' <- resolveExports locals exports
      return $ modul
          { Module.exports = exports'
          , Module.body    = body canonicalDecls
          }
  where
    locals :: [Var.Value]
    locals = concatMap declToValue decls

    body :: [D.CanonicalDecl] -> Module.CanonicalBody
    body decls =
      Module.CanonicalBody
         { program =
               let expr = Transform.toExpr (Module.names modul) decls
               in  Transform.sortDefs (dummyLet expr)
         , types = Map.empty
         , datatypes =
             Map.fromList [ (name,(vars,ctors)) | D.Datatype name vars ctors <- decls ]
         , fixities =
             [ (assoc,level,op) | D.Fixity assoc level op <- decls ]
         , aliases =
             Map.fromList [ (name,(tvs,alias)) | D.TypeAlias name tvs alias <- decls ]
         , ports =
             [ E.portName impl | D.Port (D.CanonicalPort impl) <- decls ]
         }


resolveExports
    :: [Var.Value]
    -> Var.Listing Var.Value
    -> Env.Canonicalizer [Doc] [Var.Value]
resolveExports fullList (Var.Listing partialList open) =
    if open
      then return fullList
      else do
        valueExports <- getValueExports
        aliasExports <- concat `fmap` mapM getAliasExport aliases
        adtExports <- mapM getAdtExport adts
        return $ valueExports ++ aliasExports ++ adtExports
    where
      (allValues, allAliases, allAdts) = splitValues fullList

      (values, aliases, adts) = splitValues partialList

      getValueExports =
        case Set.toList (Set.difference values' allValues') of
          [] -> return (map Var.Value values)
          xs -> notFound xs
        where
          allValues' = Set.fromList allValues
          values' = Set.fromList values

      getAliasExport alias
          | alias `elem` allAliases =
              let recordConstructor =
                      if alias `elem` allValues then [Var.Value alias] else []
              in
                  return $ Var.Alias alias : recordConstructor

          | otherwise =
              case List.find (\(name, _ctors) -> name == alias) allAdts of
                Nothing -> notFound [alias]
                Just (name, _ctor) ->
                    return [Var.Union name (Var.Listing [] False)]

      getAdtExport (name, Var.Listing ctors open) =
        case lookup name allAdts of
          Nothing -> notFound [name]
          Just (Var.Listing allCtors _)
            | open -> return $ Var.Union name (Var.Listing allCtors False)
            | otherwise ->
                case filter (`notElem` allCtors) ctors of
                  [] -> return $ Var.Union name (Var.Listing ctors False)
                  unfoundCtors -> notFound unfoundCtors

      notFound xs =
          throwError [ P.text "Export Error: trying to export non-existent values:" <+>
                       commaSep (map pretty xs)
                     ]


{-| Split a list of values into categories so we can work with them
independently.
-}
splitValues :: [Var.Value] -> ([String], [String], [(String, Var.Listing String)])
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



declToValue :: D.ValidDecl -> [Var.Value]
declToValue decl =
    case decl of
      D.Definition (Valid.Definition pattern _ _) ->
          map Var.Value (P.boundVarList pattern)

      D.Datatype name _tvs ctors ->
          [ Var.Union name (Var.Listing (map fst ctors) False) ]

      D.TypeAlias name _ tipe ->
          case tipe of
            Type.Record _ _ ->
                [ Var.Alias name, Var.Value name ]
            _ -> [ Var.Alias name ]

      _ -> []


declaration
    :: Env.Environment
    -> D.ValidDecl
    -> Env.Canonicalizer [Doc] D.CanonicalDecl
declaration env decl =
    let canonicalize kind context pattern env v =
            let ctx = P.text ("Error in " ++ context) <+> pretty pattern <> P.colon
                throw err = P.vcat [ ctx, P.text err ]
            in
                Env.onError throw (kind env v)
    in
    case decl of
      D.Definition (Valid.Definition p e t) ->
          do  p' <- canonicalize pattern "definition" p env p
              e' <- expression env e
              t' <- T.traverse (canonicalize Canonicalize.tipe "definition" p env) t
              return $ D.Definition (Canonical.Definition p' e' t')

      D.Datatype name tvars ctors ->
          D.Datatype name tvars <$> mapM canonicalize' ctors
        where
          canonicalize' (ctor,args) =
              (,) ctor <$> mapM (canonicalize Canonicalize.tipe "datatype" name env) args

      D.TypeAlias name tvars expanded ->
          do  expanded' <- canonicalize Canonicalize.tipe "type alias" name env expanded
              return (D.TypeAlias name tvars expanded')

      D.Port validPort ->
          do  Env.uses ["Native","Port"]
              Env.uses ["Native","Json"]
              case validPort of
                D.In name tipe ->
                    do  tipe' <- canonicalize Canonicalize.tipe "port" name env tipe
                        port' <- Port.check name Nothing tipe'
                        return (D.Port port')


                D.Out name expr tipe ->
                    do  expr' <- expression env expr
                        tipe' <- canonicalize Canonicalize.tipe "port" name env tipe
                        port' <- Port.check name (Just expr') tipe'
                        return (D.Port port')

      D.Fixity assoc prec op ->
          return $ D.Fixity assoc prec op


expression
    :: Env.Environment
    -> Valid.Expr
    -> Env.Canonicalizer [Doc] Canonical.Expr
expression env (A.A ann validExpr) =
    let go = expression env
        tipe' environ = format . Canonicalize.tipe environ
        throw err =
            P.vcat
              [ P.text "Error" <+> pretty ann <> P.colon
              , P.text err
              ]
        format = Env.onError throw
    in
    A.A ann <$>
    case validExpr of
      Literal lit ->
          return (Literal lit)

      Range lowExpr highExpr ->
          Range <$> go lowExpr <*> go highExpr

      Access record field ->
          Access <$> go record <*> return field

      Remove record field ->
          flip Remove field <$> go record

      Insert record field expr ->
          flip Insert field <$> go record <*> go expr

      Modify record fields ->
          Modify
            <$> go record
            <*> mapM (\(field,expr) -> (,) field <$> go expr) fields

      Record fields ->
          Record
            <$> mapM (\(field,expr) -> (,) field <$> go expr) fields

      Binop (Var.Raw op) leftExpr rightExpr ->
          do  op' <- format (Canonicalize.variable env op)
              Binop op' <$> go leftExpr <*> go rightExpr

      Lambda arg body ->
          let env' = Env.addPattern arg env
          in
              Lambda <$> format (pattern env' arg) <*> expression env' body

      App func arg ->
          App <$> go func <*> go arg

      MultiIf branches ->
          MultiIf <$> mapM go' branches
        where
          go' (condition, branch) =
              (,) <$> go condition <*> go branch

      Let defs body ->
          Let <$> mapM rename' defs <*> expression env' body
        where
          env' =
              foldr Env.addPattern env $ map (\(Valid.Definition p _ _) -> p) defs

          rename' (Valid.Definition p body mtipe) =
              Canonical.Definition
                  <$> format (pattern env' p)
                  <*> expression env' body
                  <*> T.traverse (tipe' env') mtipe

      Var (Var.Raw x) ->
          Var <$> format (Canonicalize.variable env x)

      Data name exprs ->
          Data name <$> mapM go exprs

      ExplicitList exprs ->
          ExplicitList <$> mapM go exprs

      Case expr cases ->
          Case <$> go expr <*> mapM branch cases
        where
          branch (p,b) =
              (,) <$> format (pattern env p)
                  <*> expression (Env.addPattern p env) b

      Port impl ->
          let portType pt =
                case pt of
                  Type.Normal t ->
                      Type.Normal <$> tipe' env t

                  Type.Signal root arg ->
                      Type.Signal <$> tipe' env root <*> tipe' env arg
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
          return (GLShader uid src tipe)


pattern
    :: Env.Environment
    -> P.RawPattern
    -> Env.Canonicalizer String P.CanonicalPattern
pattern env ptrn =
    case ptrn of
      P.Var x       -> return $ P.Var x
      P.Literal lit -> return $ P.Literal lit
      P.Record fs   -> return $ P.Record fs
      P.Anything    -> return P.Anything
      P.Alias x p   -> P.Alias x <$> pattern env p
      P.Data (Var.Raw name) ps ->
          P.Data <$> Canonicalize.pvar env name
                 <*> mapM (pattern env) ps
