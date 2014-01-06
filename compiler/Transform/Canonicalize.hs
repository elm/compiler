{-# OPTIONS_GHC -W #-}
module Transform.Canonicalize (interface, metadataModule) where

import Control.Arrow ((***))
import Control.Applicative (Applicative,(<$>),(<*>))
import Control.Monad.Identity
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Either as Either
import SourceSyntax.Module
import SourceSyntax.Expression
import SourceSyntax.Location as Loc
import qualified SourceSyntax.Pattern as P
import qualified SourceSyntax.Type as Type
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
    }
  where
    both f g (a,b,c,d) = (f a, b, g c, d)
    prefix name = moduleName ++ "." ++ name

    pair name = (name, moduleName ++ "." ++ name)
    canon (name,_,_,_) = pair name
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
      Type.EmptyRecord -> return tipe
      Type.Record fields ext -> Type.Record <$> mapM rnm' fields <*> rnm ext
          where rnm' (f,t) = (,) f <$> rnm t

metadataModule :: Interfaces -> MetadataModule -> Either [Doc] MetadataModule
metadataModule ifaces modul =
  do case filter (\m -> Map.notMember m ifaces) (map fst realImports) of
       [] -> Right ()
       missings -> Left [ P.text $ "The following imports were not found: " ++ List.intercalate ", " missings ]
     program' <- rename initialEnv (program modul)
     aliases' <- mapM (three4 renameType') (aliases modul)
     datatypes' <- mapM (three4 (mapM (two2 (mapM renameType')))) (datatypes modul)
     return $ modul { program = program'
                    , aliases = aliases'
                    , datatypes = datatypes' }
  where
    two2 f (a,b) = (,) a <$> f b
    three4 f (a,b,c,d) = (,,,) a b <$> f c <*> return d
    renameType' =
        Either.either (\err -> Left [P.text err]) return . renameType (replace "type" initialEnv)

    get1 (a,_,_,_) = a
    canon (name, importMethod) =
        let pair pre var = (pre ++ drop (length name + 1) var, var)
            iface = ifaces Map.! name
            allNames = concat [ Map.keys (iTypes iface)
                              , map get1 (iAliases iface)
                              , concat [ n : map fst ctors | (n,_,ctors,_) <- iAdts iface ] ]
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
    where xs = Set.toList (P.boundVars pattern)


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

rename :: Env -> LExpr -> Either [Doc] LExpr
rename env (L s expr) =
    let rnm = rename env
        throw err = Left [ P.text $ "Error " ++ show s ++ "\n" ++ err ]
        format = Either.either throw return
        renameType' env = renameType (format . replace "variable" env)
    in
    L s <$>
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
                           <*> case mtipe of
                                 Nothing -> return Nothing
                                 Just tipe -> Just <$> renameType' env' tipe

      Var x -> Var <$> format (replace "variable" env x)

      Data name es -> Data name <$> mapM rnm es

      ExplicitList es -> ExplicitList <$> mapM rnm es

      Case e cases -> Case <$> rnm e <*> mapM branch cases
          where
            branch (pattern,b) = (,) <$> format (renamePattern env pattern)
                                     <*> rename (extend env pattern) b

      Markdown uid md es -> Markdown uid md <$> mapM rnm es

      PortIn name st tt handler ->
          do st' <- renameType' env st
             handler' <- rnm handler
             return $ PortIn name st' tt handler'

      PortOut name st signal -> PortOut name <$> renameType' env st <*> rnm signal


renamePattern :: Env -> P.Pattern -> Either String P.Pattern
renamePattern env pattern =
    case pattern of
      P.PVar _ -> return pattern
      P.PLiteral _ -> return pattern
      P.PRecord _ -> return pattern
      P.PAnything -> return pattern
      P.PAlias x p -> P.PAlias x <$> renamePattern env p
      P.PData name ps -> P.PData <$> replace "pattern" env name
                                 <*> mapM (renamePattern env) ps
