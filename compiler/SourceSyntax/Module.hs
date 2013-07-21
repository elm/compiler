{-# LANGUAGE DeriveDataTypeable #-}
module SourceSyntax.Module where

import Data.Data
import Data.Binary
import Data.List (intercalate)
import qualified Data.Map as Map
import Control.Applicative ((<$>), (<*>))

import SourceSyntax.Expression
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

data ModuleInterface = ModuleInterface {
    iTypes :: Map.Map String Type,
    iAdts  :: Map.Map String (Int, [String])
} deriving Show

instance Binary ModuleInterface where
  put modul = put (iTypes modul) >> put (iAdts modul)
  get = ModuleInterface <$> get <*> get