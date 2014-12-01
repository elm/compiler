module Transform.Canonicalize (module') where

import Control.Applicative ((<$>),(<*>))
import Control.Monad.Error (runErrorT, throwError)
import Control.Monad.State (runState)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Traversable as T

import AST.Expression.General (Expr'(..), dummyLet)
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

import Transform.Canonicalize.Environment as Env
import qualified Transform.Canonicalize.Setup as Setup
import qualified Transform.Canonicalize.Type as Canonicalize
import qualified Transform.Canonicalize.Variable as Canonicalize
import qualified Transform.SortDefinitions as Transform
import qualified Transform.Declaration as Transform


module' :: Module.Interfaces -> Module.ValidModule -> Either [Doc] Module.CanonicalModule
module' interfaces modul =
    filterImports <$> modul'
  where
    (modul', usedModules) =
        runState (runErrorT (moduleHelp interfaces modul)) Set.empty

    filterImports m =
        let used (name,_) = Set.member name usedModules
        in  m { Module.imports = filter used (Module.imports m) }


moduleHelp :: Module.Interfaces -> Module.ValidModule
           -> Canonicalizer [Doc] Module.CanonicalModule
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
             [ D.portName port | D.Port port <- decls ]
         }


resolveExports :: [Var.Value] -> Var.Listing Var.Value -> Canonicalizer [Doc] [Var.Value]
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


declaration :: Environment -> D.ValidDecl -> Canonicalizer [Doc] D.CanonicalDecl
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

      D.Port port ->
          do  Env.uses ["Native","Ports"]
              Env.uses ["Native","Json"]
              D.Port <$>
                  case port of
                    D.In name t ->
                        do  t' <- canonicalize Canonicalize.tipe "port" name env t
                            return (D.In name t')
                    D.Out name e t ->
                        do  e' <- expression env e
                            t' <- canonicalize Canonicalize.tipe "port" name env t
                            return (D.Out name e' t')

      D.Fixity assoc prec op ->
          return $ D.Fixity assoc prec op


expression :: Environment -> Valid.Expr -> Canonicalizer [Doc] Canonical.Expr
expression env (A.A ann expr) =
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
    case expr of
      Literal lit ->
          return (Literal lit)

      Range e1 e2 ->
          Range <$> go e1 <*> go e2

      Access e x ->
          Access <$> go e <*> return x

      Remove e x ->
          flip Remove x <$> go e

      Insert e x v ->
          flip Insert x <$> go e <*> go v

      Modify e fs ->
          Modify <$> go e <*> mapM (\(k,v) -> (,) k <$> go v) fs

      Record fs ->
          Record <$> mapM (\(k,v) -> (,) k <$> go v) fs

      Binop (Var.Raw op) e1 e2 ->
          do  op' <- format (Canonicalize.variable env op)
              Binop op' <$> go e1 <*> go e2

      Lambda p e ->
          let env' = update p env in
          Lambda <$> format (pattern env' p) <*> expression env' e

      App e1 e2 ->
          App <$> go e1 <*> go e2

      MultiIf ps ->
          MultiIf <$> mapM go' ps
        where
          go' (b,e) = (,) <$> go b <*> go e

      Let defs e ->
          Let <$> mapM rename' defs <*> expression env' e
        where
          env' = foldr update env $ map (\(Valid.Definition p _ _) -> p) defs
          rename' (Valid.Definition p body mtipe) =
              Canonical.Definition
                  <$> format (pattern env' p)
                  <*> expression env' body
                  <*> T.traverse (tipe' env') mtipe

      Var (Var.Raw x) ->
          Var <$> format (Canonicalize.variable env x)

      Data name es ->
          Data name <$> mapM go es

      ExplicitList es ->
          ExplicitList <$> mapM go es

      Case e cases ->
          Case <$> go e <*> mapM branch cases
        where
          branch (p,b) =
              (,) <$> format (pattern env p)
                  <*> expression (update p env) b

      PortIn name st ->
          PortIn name <$> tipe' env st

      PortOut name st signal ->
          PortOut name <$> tipe' env st <*> go signal

      GLShader uid src tipe ->
          return (GLShader uid src tipe)


pattern :: Environment -> P.RawPattern -> Canonicalizer String P.CanonicalPattern
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
