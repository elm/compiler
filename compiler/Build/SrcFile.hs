{-# OPTIONS_GHC -Wall #-}
module Build.SrcFile (SrcFile, UnresolvedSrcFile, ResolvedSrcFile, getDeps, header, load, moduleId, moduleName, modulePath, path, pkg, resolve, resolvedDeps)
       where

import Control.Arrow ((&&&))
import Control.Monad.Error (ErrorT, throwError)
import Control.Monad.Trans (liftIO)
import System.FilePath ((</>))

import AST.ProgramHeader (ProgramHeader(..), ModuleName, ModuleId)
import qualified AST.ProgramHeader as ProgramHeader
import qualified Elm.Internal.Name as Package
import Parse.Helpers (iParse)
import Parse.Parse (programHeader)

-- | This module is intended to be imported qualified.

type UnresolvedSrcFile = SrcFile ()
type ResolvedSrcFile = SrcFile [ModuleId] -- ^ Includes resolved dependencies (package and module)

data SrcFile meta = SrcFile
    { _path     :: FilePath
    , _pkg      :: Maybe Package.Name
    , _header   :: ProgramHeader
    , _metadata :: meta
    } deriving (Show)

pkg :: SrcFile a -> Maybe Package.Name
pkg = _pkg

header :: SrcFile a -> ProgramHeader
header = _header

moduleName :: SrcFile a -> ModuleName
moduleName = ProgramHeader._names . header

moduleId :: SrcFile a -> ModuleId
moduleId = pkg &&& moduleName

modulePath :: SrcFile a -> FilePath
modulePath src = foldr1 (</>) (_names . header $ src)

getDeps :: SrcFile a -> [ModuleName]
getDeps = map fst . ProgramHeader._imports . header

resolvedDeps :: ResolvedSrcFile -> [ModuleId]
resolvedDeps = _metadata

path :: SrcFile a -> FilePath
path = _path

resolve :: UnresolvedSrcFile -> [ModuleId] -> ResolvedSrcFile
resolve (SrcFile pth pkg' headr _) ids = SrcFile pth pkg' headr ids

load :: Maybe Package.Name -> FilePath -> ErrorT String IO UnresolvedSrcFile
load name f = do
  txt <- liftIO $ readFile f
  case iParse programHeader txt of
    Left err -> throwError ("Error parsing file " ++ f ++ show err)
    Right hdr ->
      return $ SrcFile f name hdr ()
