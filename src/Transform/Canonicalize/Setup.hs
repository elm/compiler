{-# OPTIONS_GHC -Wall #-}
module Transform.Canonicalize.Setup
    ( environment
    , typeAliasErrorSegue
    , typeAliasErrorExplanation
    ) where

import Control.Arrow (first)
import Control.Monad (foldM)
import Control.Monad.Error (throwError)
import qualified Data.Graph as Graph
import qualified Data.List as List
import qualified Data.Map as Map

import qualified AST.Expression.Valid as Valid

import AST.Module (Interface(iAdts, iTypes, iAliases))
import qualified AST.Module as Module
import qualified AST.Type as Type
import qualified AST.Variable as Var
import qualified AST.Declaration as D
import AST.PrettyPrint (pretty, eightyCharLines)
import qualified AST.Pattern as P
import Text.PrettyPrint as P

import Transform.Canonicalize.Environment as Env
import qualified Transform.Canonicalize.Type as Canonicalize
import qualified Transform.Interface as Interface


environment :: Module.Interfaces -> Module.ValidModule -> Canonicalizer [Doc] Environment
environment interfaces modul@(Module.Module _ _ _ imports decls) =
  do () <- allImportsAvailable
     let moduleName = Module.names modul
     nonLocalEnv <- foldM (addImports moduleName interfaces) (builtIns moduleName) imports
     let (aliases, env) = List.foldl' (addDecl moduleName) ([], nonLocalEnv) decls
     addTypeAliases moduleName aliases env
  where
    allImportsAvailable :: Canonicalizer [Doc] ()
    allImportsAvailable =
        case filter (not . found) modules of
          [] -> return ()
          missings -> throwError [ P.text (missingModuleError missings) ]
        where
          modules = map fst imports

          found m = Map.member m interfaces || Module.nameIsNative m

          missingModuleError missings =
              concat [ "The following imports were not found:\n    "
                     , List.intercalate ", " (map Module.nameToString missings)
                     ]


addImports :: Module.Name -> Module.Interfaces -> Environment -> (Module.Name, Module.ImportMethod)
           -> Canonicalizer [Doc] Environment
addImports moduleName interfaces environ (name, method)
    | Module.nameIsNative name = return environ
    | otherwise =
        case method of
          Module.As name' ->
              return (updateEnviron (name' ++ "."))

          Module.Open (Var.Listing vs open)
              | open -> return (updateEnviron "")
              | otherwise -> foldM (addValue name interface) environ vs
    where
      interface = Interface.filterExports ((Map.!) interfaces name)

      updateEnviron prefix =
          let dict' = dict . map (first (prefix++)) in
          merge environ $
          Env { _home     = moduleName
              , _values   = dict' $ map pair (Map.keys (iTypes interface)) ++ ctors
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

addValue :: Module.Name -> Module.Interface -> Environment -> Var.Value
         -> Canonicalizer [Doc] Environment
addValue moduleName interface env value =
    let name = Module.nameToString moduleName
        insert' x = insert x (Var.Canonical (Var.Module moduleName) x)
        msg x = "Import Error: Could not import value '" ++ name ++ "." ++ x ++
                "'.\n    It is not exported by module " ++ name ++ "."
        notFound x = throwError [ P.text (msg x) ]
    in
    case value of
      Var.Value x
          | Map.notMember x (iTypes interface) -> notFound x
          | otherwise ->
              return $ env { _values = insert' x (_values env) }

      Var.Alias x ->
          case Map.lookup x (iAliases interface) of
            Just (tvars, t) ->
                return $ env
                    { _aliases = insert x v (_aliases env)
                    , _values = updatedValues
                    }
              where
                v = (Var.Canonical (Var.Module moduleName) x, tvars, t)
                updatedValues =
                    if Map.member x (iTypes interface)
                      then insert' x (_values env)
                      else _values env

            Nothing ->
                case Map.lookup x (iAdts interface) of
                  Nothing -> notFound x
                  Just (_,_) ->
                      return $ env { _adts = insert' x (_adts env) }

      Var.Union x (Var.Listing xs open) ->
          case Map.lookup x (iAdts interface) of
            Nothing -> notFound x
            Just (_tvars, ctors) ->
                do ctors' <- filterNames (map fst ctors)
                   return $ env { _adts = insert' x (_adts env)
                                , _values = foldr insert' (_values env) ctors'
                                , _patterns = foldr insert' (_patterns env) ctors'
                                }
                where
                  filterNames names
                      | open = return names
                      | otherwise =
                          case filter (`notElem` names) xs of
                            [] -> return names
                            c:_ -> notFound c

type Node = ((String, [String], Type.RawType), String, [String])

node :: String -> [String] -> Type.RawType -> Node
node name tvars alias = ((name, tvars, alias), name, edges alias)
    where
      edges tipe =
          case tipe of
            Type.Lambda t1 t2 -> edges t1 ++ edges t2
            Type.Var _ -> []
            Type.Type (Var.Raw x) -> [x]
            Type.App t ts -> edges t ++ concatMap edges ts
            Type.Record fs ext -> maybe [] edges ext ++ concatMap (edges . snd) fs
            Type.Aliased _ t -> edges t


addTypeAliases
    :: Module.Name
    -> [Node]
    -> Environment
    -> Canonicalizer [Doc] Environment
addTypeAliases moduleName nodes environ =
    foldM (addTypeAlias moduleName) environ (Graph.stronglyConnComp nodes)


addTypeAlias
    :: Module.Name
    -> Environment
    -> Graph.SCC (String, [String], Type.RawType)
    -> Canonicalizer [Doc] Environment
addTypeAlias moduleName env scc =
  case Graph.flattenSCC scc of
    [(name, tvars, alias)] ->
        do  alias' <- Env.onError throw (Canonicalize.tipe env alias)
            let value = (Var.Canonical (Var.Module moduleName) name, tvars, alias')
            return $ env { _aliases = insert name value (_aliases env) }
        where
          throw err =
              let msg = "Problem with type alias '" ++ name ++ "':"
              in  P.vcat [ P.text msg, P.text err ]

    aliases ->
        throwError 
          [ P.vcat
              [ P.text (eightyCharLines 0 mutuallyRecursiveMessage)
              , indented (map typeAlias aliases)
              , P.text (eightyCharLines 0 typeAliasErrorSegue)
              , indented (map datatype aliases)
              , P.text (eightyCharLines 0 typeAliasErrorExplanation)
              ]
           ]


typeAlias
    :: (String, [String], Type.Type var)
    -> D.Declaration' port def var
typeAlias (n,ts,t) =
    D.TypeAlias n ts t


datatype
    :: (String, [String], Type.Type var)
    -> D.Declaration' port def var
datatype (n,ts,t) =
    D.Datatype n ts [(n,[t])]


indented :: [D.ValidDecl] -> Doc
indented decls = P.vcat (map prty decls) <> P.text "\n"
    where
      prty decl = P.text "\n    " <> pretty decl


mutuallyRecursiveMessage :: String
mutuallyRecursiveMessage =
  "The following type aliases are mutually recursive, forming an \
  \infinite type. When you expand them, they just keep getting bigger:"


typeAliasErrorSegue :: String
typeAliasErrorSegue =
  "Try this instead:"


typeAliasErrorExplanation :: String
typeAliasErrorExplanation =
  "It looks very similar, but the 'type' keyword creates a brand new type, \
  \not just an alias for an existing one. This lets us avoid infinitely \
  \expanding it during type inference."


-- When canonicalizing, all _values should be Local, but all _adts and _patterns
-- should be fully namespaced. With _adts, they may appear in types that can
-- escape the module.
addDecl :: Module.Name -> ([Node], Environment) -> D.ValidDecl -> ([Node], Environment)
addDecl moduleName info@(nodes,env) decl =
    let namespacedVar     = Var.Canonical (Var.Module moduleName)
        addLocal      x e = insert x (Var.local     x) e
        addNamespaced x e = insert x (namespacedVar x) e
    in
    case decl of
      D.Definition (Valid.Definition pattern _ _) ->
          (,) nodes $ env
           { _values = foldr addLocal (_values env) (P.boundVarList pattern) }

      D.Datatype name _ ctors ->
          (,) nodes $ env
           { _values   = addCtors addLocal (_values env)
           , _adts     = addNamespaced name (_adts env)
           , _patterns = addCtors addNamespaced (_patterns env)
           }
        where
          addCtors how e = foldr how e (map fst ctors)

      D.TypeAlias name tvars alias ->
          (,) (node name tvars alias : nodes) $ env
           { _values = case alias of
                         Type.Record _ _ -> addLocal name (_values env)
                         _               -> _values env
           }

      D.Port port ->
          let portName = case port of
                           D.Out name _ _ -> name
                           D.In name _    -> name
          in
              (,) nodes $ env { _values = addLocal portName (_values env) }

      D.Fixity _ _ _ -> info
