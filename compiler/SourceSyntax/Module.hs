{-# LANGUAGE DeriveDataTypeable #-}
module SourceSyntax.Module where

import Data.Data
import Data.Binary
import Data.List (intercalate)
import qualified Data.Map as Map
import Control.Applicative ((<$>), (<*>))
import Control.Arrow (second)

import SourceSyntax.Expression (LExpr)
import SourceSyntax.Declaration
import SourceSyntax.Type
import System.FilePath (joinPath)
import qualified Type.Type as Type

data Module tipe var =
    Module [String] Exports Imports [Declaration tipe var]
    deriving (Show)

type Exports = [String]

type Imports = [(String, ImportMethod)]
data ImportMethod = As String | Importing [String] | Hiding [String]
                    deriving (Eq, Ord, Show, Data, Typeable)

data MetadataModule t v = MetadataModule {
    names     :: [String],
    path      :: FilePath,
    exports   :: [String],
    imports   :: [(String, ImportMethod)],
    program   :: LExpr t v,
    types     :: Map.Map String Type.Variable,
    fixities  :: [(Assoc, Int, String)],
    aliases   :: [(String, [String], Type)],
    datatypes :: [ (String, [String], [(String,[Type])]) ],
    foreignImports :: [(String, LExpr t v, String, Type)],
    foreignExports :: [(String, String, Type)]
}

type Interfaces = Map.Map String ModuleInterface
type ADT = (String, [String], [(String,[Type])])

data ModuleInterface = ModuleInterface {
    iTypes :: Map.Map String Type,
    iAdts  :: [ADT]
} deriving Show

instance Binary ModuleInterface where
  put modul = put (iTypes modul) >> put (iAdts modul)
  get = ModuleInterface <$> get <*> get


canonicalize :: String -> ImportMethod -> ModuleInterface -> ModuleInterface
canonicalize prefix importMethod interface =
    let addPrefix name = prefix ++ "." ++ name

        newName (tipe,_,_) =
            case importMethod of
              As name -> (tipe, name ++ "." ++ tipe)
              Hiding vs -> (tipe, if tipe `elem` vs then addPrefix tipe else tipe)
              Importing vs -> (tipe, if tipe `elem` vs then tipe else addPrefix tipe)
        newNames = Map.fromList . map newName $ iAdts interface

        rename name = Map.findWithDefault name name newNames

        renameADT (name, tvars, ctors) =
            (rename name, tvars, map (second (map (renameType rename))) ctors)
    in  ModuleInterface {
              iTypes = Map.map (renameType rename) (iTypes interface),
              iAdts = map renameADT (iAdts interface)
            }

renameType :: (String -> String) -> Type -> Type
renameType find tipe =
    let rnm = renameType find in
    case tipe of
      Lambda a b -> Lambda (rnm a) (rnm b)
      Var x -> Var x
      Data name ts -> Data (find name) (map rnm ts)
      EmptyRecord -> EmptyRecord
      Record fields ext -> Record (Map.map (map rnm) fields) (rnm ext)