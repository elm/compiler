{-# OPTIONS_GHC -Wall #-}
module Build.SrcFile (SrcFile(SrcFile), baseDir, relPath, toPath, make)
       where

import System.FilePath ((</>), splitFileName, splitDirectories, joinPath)

import Parse.Module (getModuleNames)

-- | This module is intended to be imported qualified.

data SrcFile = SrcFile
    { _baseDir :: FilePath -- ^ Path to top level of the file's module hierarchy
    , _relPath :: FilePath -- ^ Relative to _baseDir
    }

baseDir :: SrcFile -> FilePath
baseDir = _baseDir

relPath :: SrcFile -> FilePath
relPath = _relPath

toPath :: SrcFile -> FilePath
toPath (SrcFile dir rel) = dir </> rel

-- | Find the top level of a project and split FilePath into SrcFile components
make :: FilePath -> IO SrcFile
make f = do
  txt <- readFile f
  return $ case getModuleNames txt of
    Nothing -> uncurry SrcFile . splitFileName $ f
    Just ms ->
      let dirs = splitDirectories f
          numParents = length ms - 1
          numDirs = length dirs
          (base, rel) = splitAt ((numDirs - numParents) - 1) dirs
      in SrcFile (joinPath base) (joinPath rel)
