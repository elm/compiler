module Transform.PrettifyTypes where

import Data.List (foldl')
import Data.Map (Map)
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified AST.Module as Module
import qualified AST.Variable as Var
import qualified AST.Type as Type
import qualified Data.Map as Map
import qualified Data.Set as Set

data Import = Import
    { unqualified :: Set String
    , qualifier :: Maybe String
    } deriving (Show)

defaultExport :: Import
defaultExport = Import Set.empty Nothing

modifyWithDefault :: (Import -> Import) -> String -> Map String Import -> Map String Import
modifyWithDefault modifier key =
  Map.insertWith (\_ old -> modifier old) key (modifier defaultExport)

extractType :: Var.Value -> Maybe String
extractType value =
  case value of
    Var.Alias alias -> Just alias
    Var.ADT name _ -> Just name
    Var.Value _ -> Nothing

allImports :: Module.CanonicalModule -> Map String Import
allImports modul = foldl' insert Map.empty ls
  where
    this = (Module.nameToString $ Module.names modul, Module.open)
    ls = this : Module.imports modul
    insert m (name, im) = case im of
      Module.As qname ->
        modifyWithDefault (\r -> r { qualifier = Just qname }) name m
      Module.Open (Var.Listing expl open) ->
        case open of
          False -> Map.insert name defaultExport m
          True ->
            let types = Set.fromList $ catMaybes $ map extractType expl
            in modifyWithDefault (\r -> r { unqualified = types }) name m

qualifiedValue :: String -> Import -> String
qualifiedValue val imp =
  case qualifier imp of
    Nothing -> val
    Just ql ->
      case Set.member val (unqualified imp) of
        True -> val
        False -> ql ++ "." ++ val

prettifyVar :: Map String Import -> Var.Canonical -> String
prettifyVar env (Var.Canonical home name) =
  case home of
    Var.Module mod ->
      case Map.lookup mod env of
        Nothing -> mod ++ "." ++ name
        Just imp -> qualifiedValue name imp
    Var.Local -> name
    Var.BuiltIn -> name

prettifyType :: Map String Import -> Type.CanonicalType -> Type.RawType
prettifyType env = go
  where
    go :: Type.CanonicalType -> Type.RawType
    go typ =
      case typ of
        Type.Lambda t1 t2 -> Type.Lambda (go t1) (go t2)
        Type.Var x -> Type.Var x -- not just "typ" because types are different
        Type.Type var -> Type.Type $ Var.Raw $ prettifyVar env var
        Type.App fn args -> Type.App (go fn) $ map go args
        Type.Record fields ext -> Type.Record (map f fields) $ fmap go ext
          where f (x, y) = (x, go y)
        Type.Aliased var _ -> Type.Type $ Var.Raw $ prettifyVar env var
