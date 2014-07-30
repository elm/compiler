module AST.ProgramHeader where

import qualified Data.List as List
import System.FilePath ((</>))

import qualified AST.Module as M
import qualified AST.Variable as Var
import qualified Elm.Internal.Name as Package

type ModuleName = [String] -- | Invariant: Nonempty
type ModuleId   = (Maybe Package.Name, ModuleName)

toName :: ModuleName -> String
toName = List.intercalate "."

modulePath :: ModuleName -> FilePath
modulePath = foldr1 (</>) 

data ProgramHeader = ProgramHeader
    { _names :: ModuleName
    , _exports :: Var.Listing Var.Value
    , _docComment :: Maybe String
    , _imports :: [(ModuleName, M.ImportMethod)]
    } deriving (Show)

