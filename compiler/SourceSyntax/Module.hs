{-# LANGUAGE DeriveDataTypeable #-}
module SourceSyntax.Module where

import Data.Data
import Data.List (intercalate)
import qualified Data.Map as Map

import SourceSyntax.Expression
import SourceSyntax.Declaration
import SourceSyntax.Type
import System.FilePath (joinPath)
import qualified Type.Type as T

data Module tipe var =
    Module [String] Exports Imports [Declaration tipe var]
    deriving (Show)

type Exports = [String]

type Imports = [(String, ImportMethod)]
data ImportMethod = As String | Importing [String] | Hiding [String]
                    deriving (Eq, Ord, Show, Data, Typeable)

data MetadataModule t v = MetadataModule {
    names    :: [String],
    path     :: FilePath,
    exports  :: [String],
    imports  :: [(String, ImportMethod)],
    defs     :: [Def t v],
    types    :: Map.Map String T.Variable,
    fixities :: [(Assoc, Int, String)],
    aliases  :: [(String, [String], Type)],
    foreignImports :: [(String, LExpr t v, String, Type)],
    foreignExports :: [(String, String, Type)]
}
