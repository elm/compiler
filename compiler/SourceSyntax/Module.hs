{-# LANGUAGE DeriveDataTypeable #-}
module SourceSyntax.Module where

import Data.Data
import Data.List (intercalate)
import qualified SourceSyntax.Declaration as Decl
import System.FilePath (joinPath)
import Types.Types

data Module = Module [String] Exports Imports [Decl.Declaration]
              deriving (Show)

type Exports = [String]

type Imports = [(String, ImportMethod)]
data ImportMethod = As String | Importing [String] | Hiding [String]
                    deriving (Eq, Ord, Show, Data, Typeable)

name :: Module -> String
name (Module names _ _ _) = intercalate "." names

path :: Module -> FilePath
path (Module names _ _ _) = joinPath names