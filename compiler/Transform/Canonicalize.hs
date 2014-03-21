{-# OPTIONS_GHC -Wall #-}
module Transform.Canonicalize (interface, metadataModule) where

import Control.Arrow ((***))
import Control.Applicative (Applicative,(<$>),(<*>))
import Control.Monad.Identity
import qualified Data.Either as Either
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Traversable as T
import SourceSyntax.Annotation as A
import SourceSyntax.Expression
import SourceSyntax.Module
import SourceSyntax.PrettyPrint (pretty)
import qualified SourceSyntax.Pattern as P
import qualified SourceSyntax.Type as Type
import qualified SourceSyntax.Variable as Var
import Text.PrettyPrint as P

interface :: String -> ModuleInterface -> ModuleInterface
interface moduleName iface =
    ModuleInterface
    { iVersion = iVersion iface
    , iTypes = Map.mapKeys prefix (Map.map renameType' (iTypes iface))
    , iImports = iImports iface
    , iAdts = map (both prefix renameCtors) (iAdts iface)
    , iAliases = map (both prefix renameType') (iAliases iface)
    , iFixities = iFixities iface -- cannot have canonicalized operators while parsing
    , iPorts = iPorts iface
    }
  where
    both f g (a,b,c) = (f a, b, g c)
    prefix name = moduleName ++ "." ++ name

    pair name = (name, moduleName ++ "." ++ name)
    canon (name,_,_) = pair name
    canons = Map.fromList $ concat
             [ map canon (iAdts iface), map canon (iAliases iface) ]

    renameCtors ctors =
        map (prefix *** map renameType') ctors
    renameType' =
        runIdentity . renameType (\name -> return $ Map.findWithDefault name name canons)

renameType :: (Applicative m, Monad m) => (String -> m String) -> Type.Type -> m Type.Type
renameType renamer tipe =
    let rnm = renameType renamer in
    case tipe of
      Type.Lambda a b -> Type.Lambda <$> rnm a <*> rnm b
      Type.Var _ -> return tipe
      Type.Data name ts -> Type.Data <$> renamer name <*> mapM rnm ts
      Type.Record fields ext -> Type.Record <$> mapM rnm' fields <*> return ext
          where rnm' (f,t) = (,) f <$> rnm t

metadataModule :: Interfaces -> MetadataModule -> Either [Doc] MetadataModule
metadataModule ifaces modul =
  do case filter (\m -> Map.notMember m ifaces) (map fst realImports) of
       [] -> Right ()
       missings -> Left [ P.text $ "The following imports were not found: " ++ List.intercalate ", " missings ++
                                   "\n    You may need to compile with the --make flag to detect modules you have written."
                        ]
     program' <- rename initialEnv (program modul)
     aliases' <- mapM (three3 renameType') (aliases modul)
     datatypes' <- mapM (three3 (mapM (two2 (mapM renameType')))) (datatypes modul)
     return $ modul { program = program'
                    , aliases = aliases'
                    , datatypes = datatypes' }
  where
    two2 f (a,b) = (,) a <$> f b
    three3 f (a,b,c) = (,,) a b <$> f c
    renameType' =
        Either.either (\err -> Left [P.text err]) return . renameType (replace "type" initialEnv)

    get1 (a,_,_) = a
    canon (name, importMethod) =
        let pair pre var = (pre ++ drop (length name + 1) var, var)
            iface = ifaces Map.! name
            allNames = concat [ Map.keys (iTypes iface)
                              , map get1 (iAliases iface)
                              , concat [ n : map fst ctors | (n,_,ctors) <- iAdts iface ] ]
        in  case importMethod of
              As alias -> map (pair (alias ++ ".")) allNames
              Hiding vars -> map (pair "") $ filter (flip Set.notMember vs) allNames
                  where vs = Set.fromList vars
              Importing vars -> map (pair "") $ filter (flip Set.member vs) allNames
                  where vs = Set.fromList $ map (\v -> name ++ "." ++ v) vars

    two n = (n,n)
    localEnv = map two (map get1 (aliases modul) ++ map get1 (datatypes modul))
    globalEnv =
        map two $ ["_List",saveEnvName,"::","[]","Int","Float","Char","Bool","String"] ++
                  map (\n -> "_Tuple" ++ show (n :: Int)) [0..9]
    realImports = filter (not . List.isPrefixOf "Native." . fst) (imports modul)
    initialEnv = Map.fromList (concatMap canon realImports ++ localEnv ++ globalEnv)

type Env = Map.Map String String

extend :: Env -> P.Pattern -> Env
extend env pattern = Map.union (Map.fromList (zip xs xs)) env
    where xs = P.boundVarList pattern

replace :: String -> Env -> String -> Either String String
replace variable env v =
    if List.isPrefixOf "Native." v then return v else
    case Map.lookup v env of
      Just v' -> return v'
      Nothing -> Left $ "Could not find " ++ variable ++ " '" ++ v ++ "'." ++ msg
          where
            matches = filter (List.isInfixOf v) (Map.keys env)
            msg = if null matches then "" else
                      "\nClose matches include: " ++ List.intercalate ", " matches

-- TODO: Var.Raw -> Var.Canonical
rename :: Env -> Expr -> Either [Doc] Expr
rename env (A ann expr) =
    let rnm = rename env
        throw err = Left [ P.vcat [ P.text "Error" <+> pretty ann <> P.colon
                                  , P.text err
                                  ]
                         ]
        format = Either.either throw return
        renameType' environ = renameType (format . replace "variable" environ)
    in
    A ann <$>
    case expr of
      Literal _ -> return expr

      Range e1 e2 -> Range <$> rnm e1 <*> rnm e2

      Access e x -> Access <$> rnm e <*> return x

      Remove e x -> flip Remove x <$> rnm e

      Insert e x v -> flip Insert x <$> rnm e <*> rnm v

      Modify e fs ->
          Modify <$> rnm e <*> mapM (\(k,v) -> (,) k <$> rnm v) fs

      Record fs -> Record <$> mapM (\(k,v) -> (,) k <$> rnm v) fs

      Binop op e1 e2 ->
          do op' <- format (replace "variable" env op)
             Binop op' <$> rnm e1 <*> rnm e2

      Lambda pattern e ->
          let env' = extend env pattern in
          Lambda <$> format (renamePattern env' pattern) <*> rename env' e

      App e1 e2 -> App <$> rnm e1 <*> rnm e2

      MultiIf ps -> MultiIf <$> mapM grnm ps
              where grnm (b,e) = (,) <$> rnm b <*> rnm e

      Let defs e -> Let <$> mapM rename' defs <*> rename env' e
          where
            env' = foldl extend env $ map (\(Definition p _ _) -> p) defs
            rename' (Definition p body mtipe) =
                Definition <$> format (renamePattern env' p)
                           <*> rename env' body
                           <*> T.traverse (renameType' env') mtipe

      -- TODO: Raw -> Canonical
      Var (Var.Raw x) -> rawVar <$> format (replace "variable" env x)

      Data name es -> Data name <$> mapM rnm es

      ExplicitList es -> ExplicitList <$> mapM rnm es

      Case e cases -> Case <$> rnm e <*> mapM branch cases
          where
            branch (pattern,b) = (,) <$> format (renamePattern env pattern)
                                     <*> rename (extend env pattern) b

      Markdown uid md es -> Markdown uid md <$> mapM rnm es

      PortIn name st -> PortIn name <$> renameType' env st

      PortOut name st signal -> PortOut name <$> renameType' env st <*> rnm signal


renamePattern :: Env -> P.Pattern -> Either String P.Pattern
renamePattern env pattern =
    case pattern of
      P.Var _ -> return pattern
      P.Literal _ -> return pattern
      P.Record _ -> return pattern
      P.Anything -> return pattern
      P.Alias x p -> P.Alias x <$> renamePattern env p
      P.Data name ps -> P.Data <$> replace "pattern" env name
                               <*> mapM (renamePattern env) ps
