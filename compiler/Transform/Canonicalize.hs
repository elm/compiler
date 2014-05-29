{-# OPTIONS_GHC -W #-}
module Transform.Canonicalize (module', filterExports) where

import Control.Applicative ((<$>),(<*>))
import Control.Arrow (first)
import Control.Monad (foldM)
import qualified Data.Either as Either
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Traversable as T

import AST.Expression.General (Expr'(..), dummyLet)
import qualified AST.Expression.Valid as Valid
import qualified AST.Expression.Canonical as Canonical

import AST.Module (Interface(iAdts, iTypes, iAliases), CanonicalBody(..))
import qualified AST.Module as Module
import qualified AST.Type as Type
import qualified AST.Variable as Var
import qualified AST.Annotation as A
import qualified AST.Declaration as D
import AST.PrettyPrint (pretty, commaSep)
import qualified AST.Pattern as P
import Text.PrettyPrint as P

import Transform.Canonicalize.Environment as Env
import qualified Transform.Canonicalize.Type as Canonicalize
import qualified Transform.Canonicalize.Variable as Canonicalize
import qualified Transform.SortDefinitions as Transform
import qualified Transform.Declaration as Transform

environment :: Module.Interfaces -> Module.ValidModule -> Either [Doc] Environment
environment interfaces m@(Module.Module _ _ _ ims decls) =
  do () <- allImportsAvailable interfaces ims
     nonLocalEnv <- foldM (toEnv interfaces) Env.builtIns ims
     let moduleName = Module.getName m
     foldM (addDecl moduleName) nonLocalEnv decls

toEnv :: Module.Interfaces -> Environment -> (String, Module.ImportMethod)
      -> Either [Doc] Environment
toEnv interfaces environ (name,method)
    | List.isPrefixOf "Native." name = return environ
    | otherwise =
        case method of
          Module.As name' ->
              return (updateEnviron (name' ++ "."))

          Module.Open (Var.Listing vs open)
              | open -> return (updateEnviron "")
              | otherwise -> foldM (addValue name interface) environ vs
    where
      interface = (Map.!) interfaces name

      updateEnviron prefix =
          let dict' = dict . map (first (prefix++)) in
          merge environ $
          Env { _values   = dict' $ map pair (Map.keys (iTypes interface)) ++ ctors
              , _adts     = dict' $ map pair (Map.keys (iAdts interface))
              , _aliases  = dict' $ map alias (Map.toList (iAliases interface))
              , _patterns = dict' $ ctors
              }

      canonical :: String -> Var.Canonical
      canonical = Var.Canonical (Var.Module name)

      pair :: String -> (String, Var.Canonical)
      pair key = (key, canonical key)

      alias (x,(tvars,tipe)) = (x, (canonical x, tvars, tipe))

      ctors = concatMap (map (pair . fst) . snd . snd) (Map.toList (iAdts interface))

addValue :: String -> Module.Interface -> Environment -> Var.Value
         -> Either [Doc] Environment
addValue name interface env value =
    let insert' x = insert x (Var.Canonical (Var.Module name) x)
        msg x = "Import Error: Could not import value '" ++ name ++ "." ++ x ++
                "'.\n    It is not exported by module " ++ name ++ "."
        notFound x = Left [ P.text (msg x) ]
    in
    case value of
      Var.Value x
          | Map.notMember x (iTypes interface) -> notFound x
          | otherwise ->
              return $ env { _values = insert' x (_values env) }

      Var.Alias x ->
          case Map.lookup x (iAliases interface) of
            Just (tvars, t) ->
                let v = (Var.Canonical (Var.Module name) x, tvars, t) in
                return $ env { _aliases = insert x v (_aliases env) }
            Nothing ->
                case Map.lookup x (iAdts interface) of
                  Nothing -> notFound x
                  Just (_,_) ->
                      return $ env { _adts = insert' x (_adts env) }

      Var.ADT x (Var.Listing xs open) ->
          case Map.lookup x (iAdts interface) of
            Nothing -> notFound x
            Just (_tvars, ctors) ->
                do ctors <- filterNames (map fst ctors)
                   return $ env { _adts = insert' x (_adts env)
                                , _values = foldr insert' (_values env) ctors
                                , _patterns = foldr insert' (_patterns env) ctors
                                }
                where
                  filterNames names
                      | open = return names
                      | otherwise =
                          case filter (`notElem` names) xs of
                            [] -> return names
                            c:_ -> notFound c

-- When canonicalizing, all _values should be Local, but all _adts and _patterns
-- should be fully namespaced. With _adts, they may appear in types that can
-- escape the module.
addDecl :: String -> Environment -> D.ValidDecl -> Either [Doc] Environment
addDecl moduleName env decl =
    let namespacedVar     = Var.Canonical (Var.Module moduleName)
        addLocal      x e = insert x (Var.local     x) e
        addNamespaced x e = insert x (namespacedVar x) e
    in
    case decl of
      D.Definition (Valid.Definition pattern _ _) ->
          return $ env
           { _values = foldr addLocal (_values env) (P.boundVarList pattern) }

      D.Datatype name _ ctors ->
          return $ env
           { _values   = addCtors addLocal (_values env)
           , _adts     = addNamespaced name (_adts env)
           , _patterns = addCtors addNamespaced (_patterns env)
           }
        where
          addCtors how e = foldr how e (map fst ctors)

      D.TypeAlias name tvars alias ->
          do alias' <- Either.either throw return (Canonicalize.tipe env alias)
             return $ env
              { _aliases = insert name (namespacedVar name, tvars, alias') (_aliases env)
              , _values = case alias of
                            Type.Record _ _ -> addLocal name (_values env)
                            _               -> _values env
              }
          where
            throw err =
                let msg = "Error in type alias '" ++ name ++ "':"
                in  Left [ P.vcat [ P.text msg, P.text err ] ]

      D.Port _ -> return env

      D.Fixity _ _ _ -> return env

module' :: Module.Interfaces -> Module.ValidModule -> Either [Doc] Module.CanonicalModule
module' interfaces modul@(Module.Module _ _ exs _ decls) =
  do env <- environment interfaces modul
     canonicalDecls <- mapM (declaration env) decls
     exports' <- delist locals exs
     return $ modul { Module.exports = exports'
                    , Module.body    = body canonicalDecls
                    }
  where
    locals :: [Var.Value]
    locals = concatMap declToValue decls

    body :: [D.CanonicalDecl] -> Module.CanonicalBody
    body decls =
      Module.CanonicalBody
         { program =
               let expr = Transform.toExpr (Module.getName modul) decls
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

delist :: [Var.Value] -> Var.Listing Var.Value -> Either [Doc] [Var.Value]
delist fullList (Var.Listing partial open)
    | open = return fullList
    | otherwise = go [] (List.sort fullList) (List.sort partial)
    where
      notFound xs =
          Left $ [ P.text "Export Error: trying to export non-existent values:" <+>
                   commaSep (map pretty xs)
                 ]

      go list full partial =
        case (full, partial) of
          (_, []) -> return list
          ([], _) -> notFound partial
          (x:xs, y:ys) ->
              case (x,y) of
                (Var.Value x', Var.Value y') | x' == y' ->
                    go (x : list) xs ys

                (Var.Alias x', Var.Alias y') | x' == y' ->
                    go (x : list) xs ys

                (Var.ADT x' _, Var.Alias y') | x' == y' ->
                    go (Var.ADT x' (Var.Listing [] False) : list) xs ys

                (Var.ADT x' (Var.Listing xctors _   ),
                 Var.ADT y' (Var.Listing yctors open)) | x' == y' ->
                    if open
                    then go (x : list) xs ys
                    else case filter (`notElem` xctors) yctors of
                           [] -> go (y : list) xs ys
                           bads -> notFound bads

                _ -> go list xs partial

filterExports :: Module.Types -> [Var.Value] -> Module.Types
filterExports types values =
    Map.fromList (concatMap getValue values)
  where
    getValue :: Var.Value -> [(String, Type.CanonicalType)]
    getValue value =
        case value of
          Var.Value x -> get x
          Var.Alias x -> get x
          Var.ADT _ (Var.Listing ctors _) -> concatMap get ctors

    get :: String -> [(String, Type.CanonicalType)]
    get x =
        case Map.lookup x types of
          Just t  -> [(x,t)]
          Nothing -> []

declToValue :: D.ValidDecl -> [Var.Value]
declToValue decl =
    case decl of
      D.Definition (Valid.Definition pattern _ _) ->
          map Var.Value (P.boundVarList pattern)

      D.Datatype name _tvs ctors ->
          [ Var.ADT name (Var.Listing (map fst ctors) False) ]

      D.TypeAlias name _ (Type.Record _ _) ->
          [ Var.Alias name ]

      _ -> []

declaration :: Environment -> D.ValidDecl -> Either [Doc] D.CanonicalDecl
declaration env decl =
    let canonicalize kind context pattern env v =
            case kind env v of
              Right v' -> Right v'
              Left err -> Left [ P.vcat [ ctx, P.text err ] ]
                  where ctx = P.text ("Error in " ++ context) <+> pretty pattern <> P.colon
    in
    case decl of
      D.Definition (Valid.Definition p e t) ->
          do p' <- canonicalize pattern "definition" p env p
             e' <- expression env e
             t' <- T.traverse (canonicalize Canonicalize.tipe "definition" p env) t
             return $ D.Definition (Canonical.Definition p' e' t')

      D.Datatype name tvars ctors ->
          D.Datatype name tvars <$> mapM canonicalize' ctors
          where
            canonicalize' (ctor,args) =
                (,) ctor <$> mapM (canonicalize Canonicalize.tipe "datatype" name env) args

      D.TypeAlias name tvars expanded ->
          do expanded' <- canonicalize Canonicalize.tipe "type alias" name env expanded
             return (D.TypeAlias name tvars expanded')

      D.Port port ->
          D.Port <$> case port of
                       D.In name t ->
                           do t' <- canonicalize Canonicalize.tipe "port" name env t
                              return (D.In name t')
                       D.Out name e t ->
                           do e' <- expression env e
                              t' <- canonicalize Canonicalize.tipe "port" name env t
                              return (D.Out name e' t')

      D.Fixity assoc prec op -> return $ D.Fixity assoc prec op


allImportsAvailable :: Module.Interfaces
                    -> [(String, Module.ImportMethod)]
                    -> Either [Doc] ()
allImportsAvailable interfaces imports =
  case filter (not . found) modules of
    [] -> Right ()
    missings -> Left [ P.text (missingModuleError missings) ]
  where
    modules = map fst imports

    found m = Map.member m interfaces || List.isPrefixOf "Native." m

    missingModuleError missings =
        concat [ "The following imports were not found: "
               , List.intercalate ", " missings
               ,  "\n    You may need to compile with the --make "
               , "flag to detect modules you have written." ]

expression :: Environment -> Valid.Expr -> Either [Doc] Canonical.Expr
expression env (A.A ann expr) =
    let go = expression env
        tipe' environ = format . Canonicalize.tipe environ
        throw err =
            let msg = P.text "Error" <+> pretty ann <> P.colon
            in  Left [ P.vcat [ msg, P.text err ] ]
        format = Either.either throw return
    in
    A.A ann <$>
    case expr of
      Literal lit -> return (Literal lit)

      Range e1 e2 -> Range <$> go e1 <*> go e2

      Access e x -> Access <$> go e <*> return x

      Remove e x -> flip Remove x <$> go e

      Insert e x v -> flip Insert x <$> go e <*> go v

      Modify e fs ->
          Modify <$> go e <*> mapM (\(k,v) -> (,) k <$> go v) fs

      Record fs -> Record <$> mapM (\(k,v) -> (,) k <$> go v) fs

      Binop (Var.Raw op) e1 e2 ->
          do op' <- format (Canonicalize.variable env op)
             Binop op' <$> go e1 <*> go e2

      Lambda p e ->
          let env' = update p env in
          Lambda <$> format (pattern env' p) <*> expression env' e

      App e1 e2 -> App <$> go e1 <*> go e2

      MultiIf ps -> MultiIf <$> mapM go' ps
              where go' (b,e) = (,) <$> go b <*> go e

      Let defs e -> Let <$> mapM rename' defs <*> expression env' e
          where
            env' = foldr update env $ map (\(Valid.Definition p _ _) -> p) defs
            rename' (Valid.Definition p body mtipe) =
                Canonical.Definition
                    <$> format (pattern env' p)
                    <*> expression env' body
                    <*> T.traverse (tipe' env') mtipe

      Var (Var.Raw x) -> Var <$> format (Canonicalize.variable env x)

      Data name es -> Data name <$> mapM go es

      ExplicitList es -> ExplicitList <$> mapM go es

      Case e cases -> Case <$> go e <*> mapM branch cases
          where
            branch (p,b) = (,) <$> format (pattern env p)
                               <*> expression (update p env) b

      Markdown uid md es -> Markdown uid md <$> mapM go es

      PortIn name st -> PortIn name <$> tipe' env st

      PortOut name st signal -> PortOut name <$> tipe' env st <*> go signal

      GLShader uid src tipe -> return (GLShader uid src tipe)

pattern :: Environment -> P.RawPattern -> Either String P.CanonicalPattern
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
